use std::{
    error::Error,
    fs::{self, File},
    io::{self, ErrorKind, Read, Seek, Write},
    mem,
    path::PathBuf,
    process::ExitCode,
    str::FromStr,
};

use clap::Parser;
use pasm::{
    Expr, ExprNode, Label, Op, Pos, Reloc, RelocVal, Section, SliceInterner, StrInterner, Sym, Tok,
};
use tracing::Level;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assembly source file
    source: PathBuf,

    /// Output file (default: stdout)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Pre-defined symbols (repeatable)
    #[arg(short = 'D', long, value_name="KEY1=val", value_parser = parse_defines::<String, i32>)]
    define: Vec<(String, i32)>,

    /// One of `TRACE`, `DEBUG`, `INFO`, `WARN`, or `ERROR`
    #[arg(short, long, default_value_t = Level::INFO)]
    log_level: Level,
}

fn parse_defines<T, U>(s: &str) -> Result<(T, U), Box<dyn Error + Send + Sync + 'static>>
where
    T: FromStr,
    T::Err: Error + Send + Sync + 'static,
    U: FromStr,
    U::Err: Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid SYMBOL=value: no `=` found in `{s}`"))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}

fn main() -> ExitCode {
    let args = Args::parse();
    tracing_subscriber::fmt()
        .with_max_level(args.log_level)
        .with_writer(io::stderr)
        .init();

    if let Err(e) = main_real(args) {
        tracing::error!("{e}");
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn main_real(args: Args) -> Result<(), Box<dyn Error>> {
    let input = fs::canonicalize(args.source)?;
    let input = input.to_str().unwrap();
    let file = File::open(input).map_err(|e| format!("cant open file: {e}"))?;
    let lexer = Lexer::new(file, input);

    let mut asm = Asm::new(lexer);
    asm.str_int.intern(input); // dont forget to intern the input name
    let def_file_section = asm.str_int.intern("__DEFINES__");
    let def_unit = asm.str_int.intern("__STATIC__");
    for (name, val) in &args.define {
        let string = asm.str_int.intern(name);
        asm.syms.push(Sym::new(
            Label::new(None, string),
            Expr::Const(*val),
            def_unit,
            def_file_section,
            def_file_section,
            Pos(0, 0),
        ));
    }

    tracing::trace!("starting pass 1");
    asm.pass()?;

    tracing::trace!("starting pass 2");
    asm.rewind()?;
    asm.pass()?;

    let mut output: Box<dyn Write> = match args.output {
        Some(path) => Box::new(
            File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| format!("cant open file: {e}"))?,
        ),
        None => Box::new(io::stdout()),
    };

    tracing::trace!("writing");
    output.write_all("pasm01".as_bytes())?;
    let len = asm
        .str_int
        .storages
        .iter()
        .fold(0, |accum, storage| accum + storage.len());
    output.write_all(&len.to_le_bytes())?;
    for storage in &asm.str_int.storages {
        output.write_all(storage.as_bytes())?;
    }
    let len = asm
        .expr_int
        .storages
        .iter()
        .fold(0, |accum, storage| accum + storage.len());
    output.write_all(&len.to_le_bytes())?;
    for storage in &asm.expr_int.storages {
        for expr in storage {
            match expr {
                ExprNode::Const(value) => {
                    output.write_all(&[0])?;
                    output.write_all(&value.to_le_bytes())?;
                }
                ExprNode::Op(op) => {
                    output.write_all(&[1])?;
                    match op {
                        Op::Binary(op) => {
                            output.write_all(&[0])?;
                            output.write_all(&[op.0])?;
                        }
                        Op::Unary(op) => {
                            output.write_all(&[1])?;
                            output.write_all(&[op.0])?;
                        }
                    }
                }
                ExprNode::Label(label) => {
                    output.write_all(&[2])?;
                    if let Some(scope) = label.scope {
                        output.write_all(&[0])?;
                        let index = asm.str_int.offset(scope).unwrap();
                        output.write_all(&index.to_le_bytes())?;
                        output.write_all(&scope.len().to_le_bytes())?;
                    } else {
                        output.write_all(&[1])?;
                    }
                    let index = asm.str_int.offset(label.string).unwrap();
                    output.write_all(&index.to_le_bytes())?;
                    output.write_all(&label.string.len().to_le_bytes())?;
                }
                ExprNode::Tag(label, tag) => {
                    output.write_all(&[3])?;
                    if let Some(scope) = label.scope {
                        output.write_all(&[0])?;
                        let index = asm.str_int.offset(scope).unwrap();
                        output.write_all(&index.to_le_bytes())?;
                        output.write_all(&scope.len().to_le_bytes())?;
                    } else {
                        output.write_all(&[1])?;
                    }
                    let index = asm.str_int.offset(label.string).unwrap();
                    output.write_all(&index.to_le_bytes())?;
                    output.write_all(&label.string.len().to_le_bytes())?;
                    let index = asm.str_int.offset(tag).unwrap();
                    output.write_all(&index.to_le_bytes())?;
                    output.write_all(&tag.len().to_le_bytes())?;
                }
            }
        }
    }
    output.write_all(&asm.syms.len().to_le_bytes())?;
    for sym in &asm.syms {
        if let Some(scope) = sym.label.scope {
            output.write_all(&[0])?;
            let index = asm.str_int.offset(scope).unwrap();
            output.write_all(&index.to_le_bytes())?;
            output.write_all(&scope.len().to_le_bytes())?;
        } else {
            output.write_all(&[1])?;
        }
        let index = asm.str_int.offset(sym.label.string).unwrap();
        output.write_all(&index.to_le_bytes())?;
        output.write_all(&sym.label.string.len().to_le_bytes())?;
        match sym.value {
            Expr::Const(value) => {
                output.write_all(&[0])?;
                output.write_all(&value.to_le_bytes())?;
            }
            Expr::Addr(section, pc) => {
                output.write_all(&[1])?;
                let index = asm.str_int.offset(section).unwrap();
                output.write_all(&index.to_le_bytes())?;
                output.write_all(&section.len().to_le_bytes())?;
                output.write_all(&pc.to_le_bytes())?;
            }
            Expr::List(expr) => {
                output.write_all(&[2])?;
                let index = asm.expr_int.offset(expr).unwrap();
                output.write_all(&index.to_le_bytes())?;
                output.write_all(&expr.len().to_le_bytes())?;
            }
        }
        let index = asm.str_int.offset(sym.unit).unwrap();
        output.write_all(&index.to_le_bytes())?;
        output.write_all(&sym.unit.len().to_le_bytes())?;
        let index = asm.str_int.offset(sym.section).unwrap();
        output.write_all(&index.to_le_bytes())?;
        output.write_all(&sym.section.len().to_le_bytes())?;
        let index = asm.str_int.offset(sym.file).unwrap();
        output.write_all(&index.to_le_bytes())?;
        output.write_all(&sym.file.len().to_le_bytes())?;
        output.write_all(&sym.pos.0.to_le_bytes())?;
        output.write_all(&sym.pos.1.to_le_bytes())?;
    }
    // filter out empty sections
    let count = &asm
        .sections
        .iter()
        .filter(|section| !section.data.is_empty())
        .count();
    tracing::trace!("writing {count} sections");
    output.write_all(&count.to_le_bytes())?;
    for section in asm.sections {
        if section.data.is_empty() {
            continue;
        }
        tracing::trace!(
            "writing {} bytes of section \"{}\"",
            section.data.len(),
            section.name,
        );
        let index = asm.str_int.offset(section.name).unwrap();
        output.write_all(&index.to_le_bytes())?;
        output.write_all(&section.name.len().to_le_bytes())?;
        output.write_all(&section.data.len().to_le_bytes())?;
        output.write_all(&section.data)?;
        output.write_all(&section.relocs.len().to_le_bytes())?;
        for reloc in section.relocs {
            output.write_all(&reloc.offset.to_le_bytes())?;
            output.write_all(&reloc.width.to_le_bytes())?;
            match reloc.value {
                RelocVal::Addr(section, pc) => {
                    output.write_all(&[0])?;
                    let index = asm.str_int.offset(section).unwrap();
                    output.write_all(&index.to_le_bytes())?;
                    output.write_all(&section.len().to_le_bytes())?;
                    output.write_all(&pc.to_le_bytes())?;
                }
                RelocVal::List(expr) => {
                    output.write_all(&[1])?;
                    let index = asm.expr_int.offset(expr).unwrap();
                    output.write_all(&index.to_le_bytes())?;
                    output.write_all(&expr.len().to_le_bytes())?;
                }
            }
            let index = asm.str_int.offset(reloc.unit).unwrap();
            output.write_all(&index.to_le_bytes())?;
            output.write_all(&reloc.unit.len().to_le_bytes())?;
            let index = asm.str_int.offset(reloc.file).unwrap();
            output.write_all(&index.to_le_bytes())?;
            output.write_all(&reloc.file.len().to_le_bytes())?;
            output.write_all(&reloc.pos.0.to_le_bytes())?;
            output.write_all(&reloc.pos.1.to_le_bytes())?;
        }
    }

    tracing::debug!("symbols: {}", asm.syms.len());
    tracing::debug!(
        "string heap: {}/{} bytes",
        asm.str_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.len()),
        asm.str_int
            .storages
            .iter()
            .fold(0, |accum, storage| accum + storage.capacity())
    );
    tracing::debug!(
        "macro heap: {}/{} bytes",
        asm.tok_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.len() * mem::size_of::<MacroTok>())),
        asm.tok_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.capacity() * mem::size_of::<MacroTok>()))
    );
    tracing::debug!(
        "expr heap: {}/{} bytes",
        asm.expr_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.len() * mem::size_of::<ExprNode>())),
        asm.expr_int.storages.iter().fold(0, |accum, storage| accum
            + (storage.capacity() * mem::size_of::<ExprNode>()))
    );

    Ok(())
}

struct Asm<'a> {
    toks: Vec<Box<dyn TokStream<'a> + 'a>>,
    str_int: StrInterner<'a>,
    tok_int: SliceInterner<MacroTok<'a>>,
    expr_int: SliceInterner<ExprNode<'a>>,
    sections: Vec<Section<'a>>,
    section: usize,
    native_mode: bool,
    accum_16: bool,
    index_16: bool,
    syms: Vec<Sym<'a>>,
    scope: Option<&'a str>,
    emit: bool,
    if_level: usize,

    macros: Vec<Macro<'a>>,

    expr_buffer: Vec<ExprNode<'a>>,
    operator_buffer: Vec<Op>,
}

impl<'a> Asm<'a> {
    fn new<R: Read + Seek + 'static>(lexer: Lexer<'a, R>) -> Self {
        let mut str_int = StrInterner::new();
        let code = str_int.intern("__CODE__");
        Self {
            toks: vec![Box::new(lexer)],
            str_int,
            tok_int: SliceInterner::new(),
            expr_int: SliceInterner::new(),
            sections: vec![Section::new(code)],
            section: 0,
            native_mode: false,
            accum_16: false,
            index_16: false,
            syms: Vec::new(),
            scope: None,
            emit: false,
            if_level: 0,

            macros: Vec::new(),

            expr_buffer: Vec::new(),
            operator_buffer: Vec::new(),
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.toks.last_mut().unwrap().rewind()?;
        self.sections = vec![Section::new(self.str_int.intern("__CODE__"))];
        self.section = 0;
        self.native_mode = false;
        self.accum_16 = false;
        self.index_16 = false;
        self.scope = None;
        self.emit = true;
        self.if_level = 0;
        self.macros.clear();
        Ok(())
    }

    fn pass(&mut self) -> io::Result<()> {
        loop {
            if self.peek()? == Tok::EOF {
                if self.toks.len() <= 1 {
                    break;
                }
                self.toks.pop();
            }
            // special case, setting the PC
            if self.peek()? == Tok::STAR {
                self.eat();
                if self.peek()? != Tok::EQU {
                    return Err(self.err("expected ="));
                }
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                self.set_pc(self.range_24(expr)?);
                self.eol()?;
                continue;
            }
            if self.peek()? == Tok::IDENT {
                let mne = MNEMONICS.iter().find(|mne| self.str_like(mne.0));
                let dir = DIRECTIVES.iter().find(|dir| self.str_like(dir.0));
                // is this a label?
                if mne.is_none() && dir.is_none() && !self.str_like("?MACRO") {
                    let file = self.tok().file();
                    let pos = self.tok().pos();
                    // is this a defined macro?
                    if let Some(mac) = self
                        .macros
                        .iter()
                        .find(|mac| self.str() == mac.name)
                        .copied()
                    {
                        self.eat();
                        let mut args = Vec::new();
                        loop {
                            match self.peek()? {
                                Tok::NEWLINE | Tok::EOF => break,
                                Tok::IDENT => args.push(MacroTok::Ident(self.str_intern())),
                                Tok::STR => args.push(MacroTok::Str(self.str_intern())),
                                Tok::NUM => args.push(MacroTok::Num(self.tok().num())),
                                tok => args.push(MacroTok::Tok(tok)),
                            }
                            self.eat();
                            if self.peek()? != Tok::COMMA {
                                break;
                            }
                            self.eat();
                        }
                        self.toks.push(Box::new(MacroInvocation {
                            inner: mac,
                            index: 0,
                            args,
                            file,
                            pos,
                        }));
                        continue;
                    }

                    let string = self.str_intern();
                    let label = if !self.str().starts_with(".") {
                        self.scope.replace(string);
                        Label::new(None, string)
                    } else {
                        Label::new(self.scope, string)
                    };
                    self.eat();

                    // optional colon
                    if self.peek()? == Tok::COLON {
                        self.eat();
                    }

                    // being defined to a macro?
                    if (self.peek()? == Tok::IDENT) && self.str_like("?MACRO") {
                        if label.string.starts_with(".") {
                            return Err(self.err("macro must be global"));
                        }
                        self.eat();
                        self.macrodef(label)?;
                        self.eol()?;
                        continue;
                    }
                    let index = if let Some(item) = self
                        .syms
                        .iter()
                        .enumerate()
                        .find(|item| &item.1.label == &label)
                    {
                        // allowed to redef during second pass
                        // TODO should test if label value didnt change
                        // TODO allow variable kinds that are redefinable
                        if !self.emit {
                            return Err(self.err("symbol already defined"));
                        }
                        item.0
                    } else {
                        // save in the symbol table with temporary value
                        let index = self.syms.len();
                        let unit = self.str_int.intern("__STATIC__");
                        let section = self.sections[self.section].name;
                        self.syms.push(Sym::new(
                            label,
                            Expr::Const(0),
                            unit,
                            section,
                            self.tok().file(),
                            pos,
                        ));
                        index
                    };
                    // check if this label is being defined to a value
                    if self.peek()? == Tok::EQU {
                        self.eat();
                        let expr = self.expr()?;
                        // equ's must always be const, either on the first or second pass
                        if self.emit {
                            self.syms[index].value = Expr::Const(self.const_expr(expr)?);
                        } else if let Expr::Const(expr) = expr {
                            self.syms[index].value = Expr::Const(expr);
                        } else {
                            // we couldn't evaluate this yet, so remove it
                            self.syms.pop();
                        }
                        self.eol()?;
                        continue;
                    }
                    // otherwise it is a pointer to the current PC
                    let section = self.sections[self.section].name;
                    self.syms[index].value = Expr::Addr(section, self.pc());
                    continue;
                }
                // if this isn't a mnemonic or directive, then its an error
                if mne.is_none() && dir.is_none() {
                    return Err(self.err("unrecognized instruction"));
                }
                // is this a directive
                if let Some(dir) = dir {
                    self.directive(*dir)?;
                    self.eol()?;
                    continue;
                }
                // must be an mnemonic
                self.eat();
                self.operand(mne.unwrap())?;
            }
            self.eol()?;
        }
        Ok(())
    }

    fn pc(&self) -> u32 {
        self.sections[self.section].pc
    }

    fn set_pc(&mut self, val: u32) {
        self.sections[self.section].pc = val;
    }

    fn add_pc(&mut self, amt: u32) -> io::Result<()> {
        let val = self.pc().wrapping_add(amt);
        if (val < self.pc()) || (val > 0x01000000) {
            return Err(self.err("pc overflow"));
        }
        self.set_pc(val);
        Ok(())
    }

    fn err(&self, msg: &str) -> io::Error {
        self.tok().err(msg)
    }

    fn str(&self) -> &str {
        self.tok().str()
    }

    fn str_like(&self, string: &str) -> bool {
        self.tok().str().eq_ignore_ascii_case(string)
    }

    fn str_intern(&mut self) -> &'a str {
        let Self {
            ref mut str_int,
            toks,
            ..
        } = self;
        let string = toks.last().unwrap().str();
        str_int.intern(string)
    }

    fn peek(&mut self) -> io::Result<Tok> {
        self.tok_mut().peek()
    }

    fn eat(&mut self) {
        self.tok_mut().eat();
    }

    fn write(&mut self, buf: &[u8]) {
        self.sections[self.section].data.extend_from_slice(buf);
    }

    fn write_str(&mut self) {
        let Self {
            ref mut sections,
            section,
            toks,
            ..
        } = self;
        sections[*section]
            .data
            .extend_from_slice(toks.last().unwrap().str().as_bytes());
    }

    fn tok(&self) -> &dyn TokStream<'a> {
        self.toks.last().unwrap().as_ref()
    }

    fn tok_mut(&mut self) -> &mut dyn TokStream<'a> {
        self.toks.last_mut().unwrap().as_mut()
    }

    fn eol(&mut self) -> io::Result<()> {
        match self.peek()? {
            Tok::NEWLINE => {
                self.eat();
                Ok(())
            }
            Tok::EOF => {
                if self.toks.len() > 1 {
                    self.toks.pop();
                }
                Ok(())
            }
            _ => Err(self.err("expected end of line")),
        }
    }

    fn const_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::List(expr) => {
                if let Some(value) = self.expr_eval(expr) {
                    Ok(value)
                } else {
                    Err(self.err("expression must be constant"))
                }
            }
            _ => Err(self.err("expression must be constant")),
        }
    }

    fn const_branch_expr(&self, expr: Expr<'_>) -> io::Result<i32> {
        match expr {
            Expr::Const(value) => Ok(value),
            Expr::Addr(section, pc) => {
                if self.sections[self.section].name == section {
                    Ok(pc as i32)
                } else {
                    Err(self.err("branch expression must be constant"))
                }
            }
            Expr::List(expr) => {
                if let Some(value) = self.expr_branch_eval(expr, true) {
                    Ok(value)
                } else {
                    Err(self.err("branch expression must be constant"))
                }
            }
        }
    }

    fn range_24(&self, value: i32) -> io::Result<u32> {
        if (value as u32) > 0x00FFFFFFu32 {
            return Err(self.err("expression >3 bytes"));
        }
        Ok(value as u32)
    }

    fn range_16(&self, value: i32) -> io::Result<u16> {
        if (value as u32) > (u16::MAX as u32) {
            return Err(self.err("expression >2 bytes"));
        }
        Ok(value as u16)
    }

    fn range_8(&self, value: i32) -> io::Result<u8> {
        if (value as u32) > (u8::MAX as u32) {
            return Err(self.err("expression >1 byte"));
        }
        Ok(value as u8)
    }

    fn expr_precedence(&self, op: Op) -> u8 {
        match op {
            Op::Unary(Tok::LPAREN) => 0xFF, // lparen is lowest precedence
            Op::Unary(_) => 0,              // other unary is highest precedence
            Op::Binary(Tok::SOLIDUS | Tok::MODULUS | Tok::STAR) => 1,
            Op::Binary(Tok::PLUS | Tok::MINUS) => 2,
            Op::Binary(Tok::ASL | Tok::ASR | Tok::LSR) => 3,
            Op::Binary(Tok::LT | Tok::LTE | Tok::GT | Tok::GTE) => 4,
            Op::Binary(Tok::EQ | Tok::NEQ) => 5,
            Op::Binary(Tok::AMP) => 6,
            Op::Binary(Tok::CARET) => 7,
            Op::Binary(Tok::PIPE) => 8,
            Op::Binary(Tok::AND) => 9,
            Op::Binary(Tok::OR) => 10,
            _ => unreachable!(),
        }
    }

    fn expr_push_apply(&mut self, op: Op) {
        while let Some(top) = self.operator_buffer.last().copied() {
            if self.expr_precedence(top) > self.expr_precedence(op) {
                break;
            }
            self.operator_buffer.pop();
            self.expr_buffer.push(ExprNode::Op(top));
        }
        self.operator_buffer.push(op);
    }

    fn expr(&mut self) -> io::Result<Expr<'a>> {
        self.expr_buffer.clear();
        self.operator_buffer.clear();
        let mut seen_val = false;
        let mut paren_depth = 0;
        // sort of a pratt/shunting-yard algorithm combo
        loop {
            match self.peek()? {
                // star is multiply or the PC (as a const, intentionally)
                Tok::STAR => {
                    if !seen_val {
                        self.expr_buffer.push(ExprNode::Const(self.pc() as i32));
                        seen_val = true;
                        self.eat();
                        continue;
                    }
                    self.expr_push_apply(Op::Binary(Tok::STAR));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // these are optionally unary
                tok @ (Tok::PLUS | Tok::MINUS | Tok::CARET | Tok::LT | Tok::GT) => {
                    if seen_val {
                        self.expr_push_apply(Op::Binary(tok));
                    } else {
                        self.expr_push_apply(Op::Unary(tok));
                    }
                    seen_val = false;
                    self.eat();
                    continue;
                }
                // always unary
                tok @ (Tok::BANG | Tok::TILDE) => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    self.expr_push_apply(Op::Unary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                #[rustfmt::skip]
                tok @ (Tok::AMP | Tok::PIPE | Tok::AND | Tok::OR | Tok::SOLIDUS | Tok::MODULUS
                       | Tok::ASL | Tok::ASR | Tok::LSR | Tok::LTE | Tok::GTE | Tok::EQ | Tok::NEQ
                      ) => {
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    self.expr_push_apply(Op::Binary(tok));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::NUM => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    self.expr_buffer.push(ExprNode::Const(self.tok().num()));
                    seen_val = true;
                    self.eat();
                    continue;
                }
                Tok::LPAREN => {
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    paren_depth += 1;
                    self.operator_buffer.push(Op::Unary(Tok::LPAREN));
                    seen_val = false;
                    self.eat();
                    continue;
                }
                Tok::RPAREN => {
                    // this rparen is probably part of the indirect address
                    if self.operator_buffer.is_empty() && (paren_depth == 0) {
                        break;
                    }
                    paren_depth -= 1;
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    loop {
                        if let Some(op) = self.operator_buffer.pop() {
                            // we apply ops until we see the start of this grouping
                            match op {
                                Op::Binary(tok) | Op::Unary(tok) if tok == Tok::LPAREN => {
                                    break;
                                }
                                _ => {}
                            }
                            self.expr_buffer.push(ExprNode::Op(op));
                        } else {
                            return Err(self.err("unbalanced parens"));
                        }
                    }
                    self.eat();
                    continue;
                }
                Tok::IDENT => {
                    // is this a macro?
                    if let Some(mac) = self
                        .macros
                        .iter()
                        .find(|mac| self.str() == mac.name)
                        .copied()
                    {
                        let file = self.tok().file();
                        let pos = self.tok().pos();
                        self.eat();
                        let mut args = Vec::new();
                        loop {
                            match self.peek()? {
                                Tok::NEWLINE | Tok::EOF => break,
                                Tok::IDENT => args.push(MacroTok::Ident(self.str_intern())),
                                Tok::STR => args.push(MacroTok::Str(self.str_intern())),
                                Tok::NUM => args.push(MacroTok::Num(self.tok().num())),
                                tok => args.push(MacroTok::Tok(tok)),
                            }
                            self.eat();
                            if self.peek()? != Tok::COMMA {
                                break;
                            }
                            self.eat();
                        }
                        self.toks.push(Box::new(MacroInvocation {
                            inner: mac,
                            index: 0,
                            args,
                            file,
                            pos,
                        }));
                        continue;
                    }
                    if seen_val {
                        return Err(self.err("expected operator"));
                    }
                    // check for possible tag lookup
                    if self.str_like("?TAG") {
                        self.eat();
                        if self.peek()? != Tok::IDENT {
                            return Err(self.err("expected label"));
                        }
                        let string = self.str_intern();
                        let label = if !string.starts_with(".") {
                            Label::new(None, string)
                        } else {
                            Label::new(self.scope, string)
                        };
                        self.eat();
                        self.expect(Tok::COMMA)?;
                        if self.peek()? != Tok::STR {
                            return Err(self.err("expected tag name"));
                        }
                        let tag = self.str_intern();
                        self.expr_buffer.push(ExprNode::Tag(label, tag));
                    } else {
                        let string = self.str_intern();
                        let label = if !string.starts_with(".") {
                            Label::new(None, string)
                        } else {
                            Label::new(self.scope, string)
                        };
                        self.expr_buffer.push(ExprNode::Label(label));
                    }
                    seen_val = true;
                    self.eat();
                    continue;
                }
                tok => {
                    // we might be in a macro or something
                    if (tok == Tok::EOF) && (self.toks.len() > 1) {
                        self.toks.pop();
                        continue;
                    }
                    if !seen_val {
                        return Err(self.err("expected value"));
                    }
                    if paren_depth != 0 {
                        return Err(self.err("unbalanced parens"));
                    }
                    break;
                }
            }
        }
        while let Some(top) = self.operator_buffer.pop() {
            self.expr_buffer.push(ExprNode::Op(top));
        }
        if let Some(value) = self.expr_eval(&self.expr_buffer) {
            Ok(Expr::Const(value))
        } else {
            Ok(Expr::List(self.expr_int.intern(&self.expr_buffer)))
        }
    }

    fn expr_eval(&self, expr: &[ExprNode<'_>]) -> Option<i32> {
        self.expr_branch_eval(expr, false)
    }

    fn expr_branch_eval(&self, expr: &[ExprNode<'_>], branch: bool) -> Option<i32> {
        let mut scratch = Vec::new();
        for node in expr.iter() {
            match *node {
                ExprNode::Const(value) => scratch.push(value),
                ExprNode::Label(label) => {
                    if let Some(sym) = self.syms.iter().find(|sym| &sym.label == &label) {
                        match sym.value {
                            Expr::Const(value) => scratch.push(value),
                            Expr::Addr(section, pc) => {
                                if branch && (self.sections[self.section].name == section) {
                                    scratch.push(pc as i32)
                                } else {
                                    // the linker has to handle this
                                    return None;
                                }
                            }
                            // expand the sub-expression recursively
                            Expr::List(expr) => {
                                scratch.push(self.expr_branch_eval(expr, branch)?);
                            }
                        }
                    } else {
                        return None; // needs to be solved later
                    }
                }
                ExprNode::Tag(_, _) => {
                    return None; // tags can only be solved at link-time
                }
                ExprNode::Op(op) => {
                    let rhs = scratch.pop().unwrap();
                    match op {
                        Op::Unary(Tok::PLUS) => scratch.push(rhs),
                        Op::Unary(Tok::MINUS) => scratch.push(-rhs),
                        Op::Unary(Tok::TILDE) => scratch.push(!rhs),
                        Op::Unary(Tok::BANG) => scratch.push((rhs == 0) as i32),
                        Op::Unary(Tok::LT) => scratch.push(((rhs as u32) & 0xFF) as i32),
                        Op::Unary(Tok::GT) => scratch.push((((rhs as u32) & 0xFF00) >> 8) as i32),
                        Op::Unary(Tok::CARET) => {
                            scratch.push((((rhs as u32) & 0xFF0000) >> 16) as i32)
                        }
                        Op::Binary(tok) => {
                            let lhs = scratch.pop().unwrap();
                            match tok {
                                Tok::PLUS => scratch.push(lhs.wrapping_add(rhs)),
                                Tok::MINUS => scratch.push(lhs.wrapping_sub(rhs)),
                                Tok::STAR => scratch.push(lhs.wrapping_mul(rhs)),
                                Tok::SOLIDUS => scratch.push(lhs.wrapping_div(rhs)),
                                Tok::MODULUS => scratch.push(lhs.wrapping_rem(rhs)),
                                Tok::ASL => scratch.push(lhs.wrapping_shl(rhs as u32)),
                                Tok::ASR => scratch.push(lhs.wrapping_shr(rhs as u32)),
                                Tok::LSR => {
                                    scratch.push((lhs as u32).wrapping_shl(rhs as u32) as i32)
                                }
                                Tok::LT => scratch.push((lhs < rhs) as i32),
                                Tok::LTE => scratch.push((lhs <= rhs) as i32),
                                Tok::GT => scratch.push((lhs > rhs) as i32),
                                Tok::GTE => scratch.push((lhs >= rhs) as i32),
                                Tok::EQ => scratch.push((lhs == rhs) as i32),
                                Tok::NEQ => scratch.push((lhs != rhs) as i32),
                                Tok::AMP => scratch.push(lhs & rhs),
                                Tok::PIPE => scratch.push(lhs | rhs),
                                Tok::CARET => scratch.push(lhs ^ rhs),
                                Tok::AND => scratch.push(((lhs != 0) && (rhs != 0)) as i32),
                                Tok::OR => scratch.push(((lhs != 0) || (rhs != 0)) as i32),
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        scratch.last().copied()
    }

    fn expect(&mut self, tok: Tok) -> io::Result<()> {
        if self.peek()? != tok {
            return Err(self.err("unexpected garbage"));
        }
        self.eat();
        Ok(())
    }

    fn reloc(&mut self, offset: usize, width: u8, expr: Expr<'a>, pos: Pos) {
        let pc = self.pc() as usize;
        let offset = pc + offset;
        let value = match expr {
            Expr::Const(_) => unreachable!(),
            Expr::Addr(section, pc) => RelocVal::Addr(section, pc),
            Expr::List(expr) => RelocVal::List(expr),
        };
        let unit = self.str_int.intern("__STATIC__");
        let file = self.tok().file();
        self.sections[self.section].relocs.push(Reloc {
            offset,
            width,
            value,
            unit,
            file,
            pos,
        });
    }

    fn operand(&mut self, mne: Mne) -> io::Result<()> {
        match mne {
            Mne::LD => {
                self.eat();
                if self.peek()? == Tok::LPAREN {
                    self.eat();
                    match self.peek()? {
                        tok @ (Tok::BC | Tok::DE) => {
                            self.eat();
                            self.expect(Tok::RPAREN)?;
                            self.expect(Tok::COMMA)?;
                            self.expect(Tok::A)?;
                            if self.emit {
                                self.write(&[match tok {
                                    Tok::BC => 0x02,
                                    Tok::DE => 0x12,
                                }])?;
                            }
                            return self.add_pc(1);
                        }
                        Tok::HL => {
                            self.eat();
                            self.expect(Tok::RPAREN)?;
                            self.expect(Tok::COMMA)?;
                            match self.peek()? {
                                tok @ (Tok::A
                                | Tok::B
                                | Tok::C
                                | Tok::D
                                | Tok::E
                                | Tok::H
                                | Tok::L) => {
                                    self.eat();
                                    if self.emit {
                                        self.write(&[match tok {
                                            Tok::A => 0x77,
                                            Tok::B => 0x70,
                                            Tok::C => 0x71,
                                            Tok::D => 0x72,
                                            Tok::E => 0x73,
                                            Tok::H => 0x74,
                                            Tok::L => 0x75,
                                        }])?;
                                    }
                                    return self.add_pc(1);
                                }
                                _ => {
                                    let pos = self.tok().pos();
                                    let expr = self.expr()?;
                                    if self.emit {
                                        self.write(&[0x36])?;
                                        if let Ok(value) = self.const_expr(expr) {
                                            self.write(&self.range_8(value)?.to_le_bytes());
                                        } else {
                                            self.write(&[0xFD]);
                                            self.reloc(1, 1, expr, pos);
                                        }
                                    }
                                    return self.add_pc(2);
                                }
                            }
                        }
                        Tok::C => {
                            self.eat();
                            self.expect(Tok::RPAREN)?;
                            self.expect(Tok::COMMA)?;
                            self.expect(Tok::A)?;
                            if self.emit {
                                self.write(&[0xE2])?;
                            }
                            return self.add_pc(1);
                        }
                        _ => {
                            let pos = self.tok().pos();
                            let expr = self.expr()?;
                            self.expect(Tok::COMMA)?;
                            if self.peek()? == Tok::SP {
                                self.eat();
                                if self.emit {
                                    self.write(&[0x08])?;
                                    if let Ok(value) = self.const_expr(expr) {
                                        self.write(&self.range_16(value)?.to_le_bytes());
                                    } else {
                                        self.write(&[0xFD, 0xFD]);
                                        self.reloc(1, 2, expr, pos);
                                    }
                                }
                                return self.add_pc(3);
                            }
                            self.expect(Tok::A)?;
                            if self.emit {
                                self.write(&[0xEA])?;
                                if let Ok(value) = self.const_expr(expr) {
                                    self.write(&self.range_16(value)?.to_le_bytes());
                                } else {
                                    self.write(&[0xFD, 0xFD]);
                                    self.reloc(1, 2, expr, pos);
                                }
                            }
                            return self.add_pc(3);
                        }
                    }
                }
                if self.peek()? == Tok::A {
                    self.eat();
                    self.expect(Tok::COMMA)?;
                    match self.peek()? {}
                }
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn directive(&mut self, dir: Dir) -> io::Result<()> {
        match dir {
            Dir::DATA8 => {
                self.eat();
                loop {
                    if self.peek()? == Tok::STR {
                        if self.emit {
                            self.write_str();
                        }
                        self.add_pc(self.str().len() as u32)?;
                        self.eat();
                    } else {
                        let pos = self.tok().pos();
                        let expr = self.expr()?;
                        if self.emit {
                            if let Ok(value) = self.const_expr(expr) {
                                self.write(&self.range_8(value)?.to_le_bytes());
                            } else {
                                self.write(&[0x42]);
                                self.reloc(0, 1, expr, pos);
                            }
                        }
                        self.add_pc(1)?;
                    }
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
            }
            Dir::DATA16 => {
                self.eat();
                loop {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_16(value)?.to_le_bytes());
                        } else {
                            self.write(&[0x42, 0x42]);
                            self.reloc(0, 2, expr, pos);
                        }
                    }
                    self.add_pc(2)?;
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
            }
            Dir::DATA24 => {
                self.eat();
                loop {
                    let pos = self.tok().pos();
                    let expr = self.expr()?;
                    if self.emit {
                        if let Ok(value) = self.const_expr(expr) {
                            self.write(&self.range_24(value)?.to_le_bytes());
                        } else {
                            self.write(&[0x42, 0x42, 0x42]);
                            self.reloc(0, 3, expr, pos);
                        }
                    }
                    self.add_pc(3)?;
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
            }
            Dir::SECTION => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected section name"));
                }
                let name = self.str_intern();
                self.eat();
                let index = if let Some(sec) = self
                    .sections
                    .iter()
                    .enumerate()
                    .find(|sec| &sec.1.name == &name)
                {
                    sec.0
                } else {
                    // save in the section table with default values
                    let index = self.sections.len();
                    self.sections.push(Section::new(name));
                    index
                };
                self.section = index;
            }
            Dir::EXPORT => {
                self.eat();
                loop {
                    if self.peek()? != Tok::IDENT {
                        return Err(self.err("expected symbol name"));
                    }
                    let string = self.str_intern();
                    if string.starts_with(".") {
                        return Err(self.err("exports must be global"));
                    }
                    if self.emit {
                        let label = Label::new(None, string);
                        let unit = self.str_int.intern("__EXPORT__");
                        if let Some(sym) = self.syms.iter_mut().find(|sym| &sym.label == &label) {
                            if sym.unit == unit {
                                return Err(self.err("symbol is already exported"));
                            }
                            sym.unit = unit;
                        } else {
                            return Err(self.err("cannot export undeclared symbol"));
                        }
                    }
                    self.eat();
                    if self.peek()? != Tok::COMMA {
                        break;
                    }
                    self.eat();
                }
            }
            Dir::PAD => {
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                let pad = self.range_24(expr)?;
                if self.emit {
                    for _ in 0..pad {
                        self.write(&[0]);
                    }
                }
                self.add_pc(pad)?;
            }
            Dir::ALIGN => {
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                let adj = self.pc() % self.range_24(expr)?;
                if self.emit {
                    for _ in 0..adj {
                        self.write(&[0]);
                    }
                }
                self.add_pc(adj)?;
            }
            Dir::INCLUDE => {
                self.eat();
                if self.peek()? != Tok::STR {
                    return Err(self.err("expected file name"));
                }
                let name = self.str();
                let name = fs::canonicalize(name)?;
                let name = self.str_int.intern(name.to_str().unwrap());
                let file = File::open(name)?;
                self.eat();
                let lexer = Lexer::new(file, name);
                self.toks.push(Box::new(lexer));
            }
            Dir::IF | Dir::IFDEF | Dir::IFNDEF => {
                self.eat();
                let expr = self.expr()?;
                let skip = match dir {
                    Dir::IF => self.const_expr(expr)? == 0,
                    Dir::IFDEF => !matches!(expr, Expr::Const(_)),
                    Dir::IFNDEF => matches!(expr, Expr::Const(_)),
                    _ => unreachable!(),
                };
                if skip {
                    let mut if_level = 0;
                    loop {
                        if self.peek()? == Tok::IDENT {
                            if self.str_like(Dir::IF.0)
                                || self.str_like(Dir::IFDEF.0)
                                || self.str_like(Dir::IFNDEF.0)
                                || self.str_like("?MACRO")
                            {
                                if_level += 1;
                            } else if self.str_like(Dir::END.0) {
                                if if_level == 0 {
                                    self.eat();
                                    return Ok(());
                                }
                                if_level -= 1;
                            }
                        }
                        self.eat();
                    }
                }
                self.if_level += 1;
            }
            Dir::END => {
                if self.if_level == 0 {
                    return Err(self.err("unexpected end"));
                }
                self.eat();
                self.if_level -= 1;
            }
            Dir::INDEX8 => {
                self.eat();
                self.index_16 = false;
            }
            Dir::INDEX16 => {
                self.eat();
                self.index_16 = true;
            }
            Dir::ACCUM8 => {
                self.eat();
                self.accum_16 = false;
            }
            Dir::ACCUM16 => {
                self.eat();
                self.accum_16 = true;
            }
            Dir::EMULATE => {
                self.eat();
                self.native_mode = false;
            }
            Dir::NATIVE => {
                self.eat();
                self.native_mode = true;
            }
            Dir::RES => {
                self.eat();
                let expr = self.expr()?;
                let expr = self.const_expr(expr)?;
                let res = self.range_24(expr)?;
                self.add_pc(res)?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }

    fn macrodef(&mut self, label: Label<'a>) -> io::Result<()> {
        self.eol()?;
        let mut toks = Vec::new();
        let mut if_level = 0;
        loop {
            if self.peek()? == Tok::IDENT {
                if self.str_like(Dir::IF.0)
                    || self.str_like(Dir::IFDEF.0)
                    || self.str_like(Dir::IFNDEF.0)
                    || self.str_like("?MACRO")
                {
                    if_level += 1;
                } else if self.str_like(Dir::END.0) {
                    if if_level == 0 {
                        self.eat();
                        // trim tailing newlines
                        while let Some(MacroTok::Tok(Tok::NEWLINE)) = toks.last() {
                            toks.pop();
                        }
                        toks.push(MacroTok::Tok(Tok::EOF));
                        break;
                    }
                    if_level -= 1;
                }
            }
            match self.peek()? {
                Tok::EOF => return Err(self.err("unexpected end of file")),
                Tok::IDENT => toks.push(MacroTok::Ident(self.str_intern())),
                Tok::STR => toks.push(MacroTok::Str(self.str_intern())),
                Tok::NUM => toks.push(MacroTok::Num(self.tok().num())),
                Tok::ARG => toks.push(MacroTok::Arg((self.tok().num() as usize) - 1)),
                tok => toks.push(MacroTok::Tok(tok)),
            }
            self.eat();
        }
        let toks = self.tok_int.intern(&toks);
        self.macros.push(Macro {
            name: label.string,
            toks,
        });
        Ok(())
    }
}

#[derive(PartialEq, Eq)]
struct Mne(&'static str); // I really only newtype these to create namespaces

impl Mne {
    const LD: Self = Self("LD");
    const LDD: Self = Self("LDD");
    const LDI: Self = Self("LDI");
    const LDH: Self = Self("LDH");
    const PUSH: Self = Self("PUSH");
    const POP: Self = Self("POP");
    const ADD: Self = Self("ADD");
    const ADC: Self = Self("ADC");
    const SUB: Self = Self("SUB");
    const SBC: Self = Self("SBC");
    const AND: Self = Self("AND");
    const OR: Self = Self("OR");
    const XOR: Self = Self("XOR");
    const CP: Self = Self("CP");
    const INC: Self = Self("INC");
    const DEC: Self = Self("DEC");
    const SWAP: Self = Self("SWAP");
    const DAA: Self = Self("DAA");
    const CPL: Self = Self("CPL");
    const CCF: Self = Self("CCF");
    const SCF: Self = Self("SCF");
    const NOP: Self = Self("NOP");
    const HALT: Self = Self("HALT");
    const STOP: Self = Self("STOP");
    const DI: Self = Self("DI");
    const EI: Self = Self("EI");
    const RLCA: Self = Self("RLCA");
    const RLA: Self = Self("RLA");
    const RRCA: Self = Self("RRCA");
    const RRA: Self = Self("RRA");
    const RLC: Self = Self("RLC");
    const RL: Self = Self("RL");
    const RRC: Self = Self("RRC");
    const RR: Self = Self("RR");
    const SLA: Self = Self("SLA");
    const SRA: Self = Self("SRA");
    const SRL: Self = Self("SRL");
    const BIT: Self = Self("BIT");
    const SET: Self = Self("SET");
    const RES: Self = Self("RES");
    const JP: Self = Self("JP");
    const JR: Self = Self("JR");
    const CALL: Self = Self("CALL");
    const RST: Self = Self("RST");
    const RET: Self = Self("RET");
    const RETI: Self = Self("RETI");
}

const MNEMONICS: &[Mne] = &[
    Mne::LD,
    Mne::LDD,
    Mne::LDI,
    Mne::LDH,
    Mne::PUSH,
    Mne::POP,
    Mne::ADD,
    Mne::ADC,
    Mne::SUB,
    Mne::SBC,
    Mne::AND,
    Mne::OR,
    Mne::XOR,
    Mne::CP,
    Mne::INC,
    Mne::DEC,
    Mne::SWAP,
    Mne::DAA,
    Mne::CPL,
    Mne::CCF,
    Mne::SCF,
    Mne::NOP,
    Mne::HALT,
    Mne::STOP,
    Mne::DI,
    Mne::EI,
    Mne::RLCA,
    Mne::RLA,
    Mne::RRCA,
    Mne::RRA,
    Mne::RLC,
    Mne::RL,
    Mne::RRC,
    Mne::RR,
    Mne::SLA,
    Mne::SRA,
    Mne::SRL,
    Mne::BIT,
    Mne::SET,
    Mne::RES,
    Mne::JP,
    Mne::JR,
    Mne::CALL,
    Mne::RST,
    Mne::RET,
    Mne::RETI,
];

#[derive(Clone, Copy, PartialEq, Eq)]
struct Dir(&'static str);

impl Dir {
    const DATA8: Self = Self("?DATA8");
    const DATA16: Self = Self("?DATA16");
    const DATA24: Self = Self("?DATA24");
    const SECTION: Self = Self("?SECTION");
    const EXPORT: Self = Self("?EXPORT");
    const PAD: Self = Self("?PAD");
    const ALIGN: Self = Self("?ALIGN");
    const INCLUDE: Self = Self("?INCLUDE");
    const IF: Self = Self("?IF");
    const IFDEF: Self = Self("?IFDEF");
    const IFNDEF: Self = Self("?IFNDEF");
    const END: Self = Self("?END");
    const INDEX8: Self = Self("?INDEX8");
    const INDEX16: Self = Self("?INDEX16");
    const ACCUM8: Self = Self("?ACCUM8");
    const ACCUM16: Self = Self("?ACCUM16");
    const EMULATE: Self = Self("?EMULATE");
    const NATIVE: Self = Self("?NATIVE");
    const RES: Self = Self("?RES");
}

const DIRECTIVES: &[Dir] = &[
    Dir::DATA8,
    Dir::DATA16,
    Dir::DATA24,
    Dir::SECTION,
    Dir::EXPORT,
    Dir::PAD,
    Dir::ALIGN,
    Dir::INCLUDE,
    Dir::IF,
    Dir::IFDEF,
    Dir::IFNDEF,
    Dir::END,
    Dir::INDEX8,
    Dir::INDEX16,
    Dir::ACCUM8,
    Dir::ACCUM16,
    Dir::EMULATE,
    Dir::NATIVE,
    Dir::RES,
];

const GRAPHEMES: &[(&[u8; 2], Tok)] = &[
    (b"<<", Tok::ASL),
    (b">>", Tok::ASR),
    (b"~>", Tok::LSR),
    (b"<=", Tok::LTE),
    (b">=", Tok::GTE),
    (b"==", Tok::EQ),
    (b"!=", Tok::NEQ),
    (b"&&", Tok::AND),
    (b"||", Tok::OR),
    (b"AF", Tok::AF),
    (b"BC", Tok::BC),
    (b"DE", Tok::DE),
    (b"HL", Tok::HL),
    (b"SP", Tok::SP),
    (b"NC", Tok::NC),
    (b"NZ", Tok::NZ),
];

trait TokStream<'a> {
    fn err(&self, msg: &str) -> io::Error;

    fn peek(&mut self) -> io::Result<Tok>;

    fn eat(&mut self);

    fn rewind(&mut self) -> io::Result<()>;

    fn str(&self) -> &str;

    fn num(&self) -> i32;

    fn file(&self) -> &'a str;

    fn pos(&self) -> Pos;
}

struct Lexer<'a, R> {
    reader: PeekReader<R>,
    string: String,
    number: i32,
    stash: Option<Tok>,
    file: &'a str,
    pos: Pos,
}

impl<'a, R: Read + Seek> Lexer<'a, R> {
    fn new(reader: R, file: &'a str) -> Self {
        Self {
            reader: PeekReader::new(reader),
            string: String::new(),
            number: 0,
            stash: None,
            file,
            pos: Pos(1, 1),
        }
    }
}

impl<'a, R: Read + Seek> TokStream<'a> for Lexer<'a, R> {
    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(
            ErrorKind::InvalidData,
            format!("{}:{}:{}: {msg}", self.file, self.pos.0, self.pos.1),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        if let Some(tok) = self.stash {
            return Ok(tok);
        }
        // skip whitespace
        while let Some(c) = self.reader.peek()? {
            if !b" \t\r".contains(&c) {
                break;
            }
            self.reader.eat();
        }
        // skip comment
        if let Some(b';') = self.reader.peek()? {
            while !matches!(self.reader.peek()?, Some(b'\n')) {
                self.reader.eat();
            }
        }
        self.pos = self.reader.pos;
        match self.reader.peek()? {
            None => {
                self.reader.eat();
                self.stash = Some(Tok::EOF);
                Ok(Tok::EOF)
            }
            // macro argument
            Some(b'\\') => {
                self.reader.eat();
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_digit() {
                        break;
                    }
                    self.string.push(c as char);
                    self.reader.eat();
                }
                self.number =
                    i32::from_str_radix(&self.string, 10).map_err(|e| self.err(&e.to_string()))?;
                if self.number < 1 {
                    return Err(self.err("macro argument must be positive"));
                }
                self.stash = Some(Tok::ARG);
                Ok(Tok::ARG)
            }
            // number
            Some(c) if c.is_ascii_digit() || c == b'$' || c == b'%' => {
                let radix = match c {
                    b'$' => {
                        self.reader.eat();
                        16
                    }
                    b'%' => {
                        self.reader.eat();
                        2
                    }
                    _ => 10,
                };
                // edge case: modulus
                if (c == b'%') && self.reader.peek()?.is_some_and(|nc| !b"01".contains(&nc)) {
                    self.stash = Some(Tok::MODULUS);
                    return Ok(Tok::MODULUS);
                }
                // parse number
                while let Some(c) = self.reader.peek()? {
                    if c == b'_' {
                        continue; // allow '_' separators in numbers
                    }
                    if !c.is_ascii_alphanumeric() {
                        break;
                    }
                    self.string.push(c as char);
                    self.reader.eat();
                }
                self.number = i32::from_str_radix(&self.string, radix)
                    .map_err(|e| self.err(&e.to_string()))?;
                self.stash = Some(Tok::NUM);
                Ok(Tok::NUM)
            }
            // string
            Some(b'"') => {
                self.reader.eat();
                while let Some(c) = self.reader.peek()? {
                    if c == b'"' {
                        self.reader.eat();
                        break;
                    }
                    self.string.push(c as char);
                    self.reader.eat();
                }
                self.stash = Some(Tok::STR);
                Ok(Tok::STR)
            }
            // char
            Some(b'\'') => {
                self.reader.eat();
                if let Some(c) = self.reader.peek()? {
                    if c.is_ascii_graphic() {
                        self.reader.eat();
                        self.number = c as i32;
                        self.stash = Some(Tok::NUM);
                        return Ok(Tok::NUM);
                    }
                }
                Err(self.err("unexpected garbage"))
            }
            // directives, idents, and single chars
            Some(c) => {
                while let Some(c) = self.reader.peek()? {
                    if !c.is_ascii_alphanumeric() && !b"_.?".contains(&c) {
                        break;
                    }
                    self.reader.eat();
                    self.string.push(c as char);
                }
                // check for grapheme
                if let Some(nc) = self.reader.peek()? {
                    let s = &[c.to_ascii_uppercase(), nc.to_ascii_uppercase()];
                    if let Some(tok) = GRAPHEMES
                        .iter()
                        .find_map(|(bs, tok)| (*bs == s).then_some(tok))
                        .copied()
                    {
                        self.reader.eat();
                        self.stash = Some(tok);
                        return Ok(tok);
                    }
                }
                // must be an identifier
                if self.string.len() > 1 {
                    self.stash = Some(Tok::IDENT);
                    return Ok(Tok::IDENT);
                }
                // the char wasn't an ident, so wasnt eaten
                if self.string.len() == 0 {
                    self.reader.eat();
                }
                // else return an uppercase of whatever this char is
                self.stash = Some(Tok(c.to_ascii_uppercase()));
                Ok(Tok(c.to_ascii_uppercase()))
            }
        }
    }

    fn eat(&mut self) {
        self.string.clear();
        self.stash.take();
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.string.clear();
        self.stash = None;
        self.pos = Pos(1, 1);
        self.reader.rewind()
    }

    fn str(&self) -> &str {
        &self.string
    }

    fn num(&self) -> i32 {
        self.number
    }

    fn file(&self) -> &'a str {
        &self.file
    }

    fn pos(&self) -> Pos {
        self.pos
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MacroTok<'a> {
    Tok(Tok),
    Str(&'a str),
    Ident(&'a str),
    Num(i32),
    Arg(usize),
}

#[derive(Clone, Copy)]
struct Macro<'a> {
    name: &'a str,
    toks: &'a [MacroTok<'a>],
}

struct MacroInvocation<'a> {
    inner: Macro<'a>,
    index: usize,
    args: Vec<MacroTok<'a>>,
    file: &'a str,
    pos: Pos,
}

impl<'a> TokStream<'a> for MacroInvocation<'a> {
    fn err(&self, msg: &str) -> io::Error {
        io::Error::new(
            ErrorKind::InvalidData,
            format!(
                "{}:{}:{}: in macro \"{}\": {msg}",
                self.file, self.pos.0, self.pos.1, self.inner.name
            ),
        )
    }

    fn peek(&mut self) -> io::Result<Tok> {
        match self.inner.toks[self.index] {
            MacroTok::Tok(tok) => Ok(tok),
            MacroTok::Str(_) => Ok(Tok::STR),
            MacroTok::Ident(_) => Ok(Tok::IDENT),
            MacroTok::Num(_) => Ok(Tok::NUM),
            MacroTok::Arg(index) => {
                if index >= self.args.len() {
                    return Err(self.err("argument is undefined"));
                }
                match self.args[index] {
                    MacroTok::Tok(tok) => Ok(tok),
                    MacroTok::Str(_) => Ok(Tok::STR),
                    MacroTok::Ident(_) => Ok(Tok::IDENT),
                    MacroTok::Num(_) => Ok(Tok::NUM),
                    _ => unreachable!(),
                }
            }
        }
    }

    fn eat(&mut self) {
        self.index += 1;
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.index = 0;
        Ok(())
    }

    fn str(&self) -> &str {
        match self.inner.toks[self.index] {
            MacroTok::Str(string) => string,
            MacroTok::Ident(string) => string,
            MacroTok::Arg(index) => match self.args[index] {
                MacroTok::Str(string) => string,
                MacroTok::Ident(string) => string,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn num(&self) -> i32 {
        match self.inner.toks[self.index] {
            MacroTok::Num(val) => val,
            MacroTok::Arg(index) => match self.args[index] {
                MacroTok::Num(val) => val,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn file(&self) -> &'a str {
        &self.file
    }

    fn pos(&self) -> Pos {
        self.pos
    }
}

struct PeekReader<R> {
    inner: R,
    pos: Pos,
    stash: Option<u8>,
}

impl<R: Read + Seek> PeekReader<R> {
    fn new(reader: R) -> Self {
        Self {
            inner: reader,
            pos: Pos(1, 1),
            stash: None,
        }
    }

    fn peek(&mut self) -> io::Result<Option<u8>> {
        if self.stash.is_none() {
            let mut buf = [0];
            self.stash = self
                .inner
                .read(&mut buf)
                .map(|n| if n == 0 { None } else { Some(buf[0]) })?;
        }
        Ok(self.stash)
    }

    fn eat(&mut self) {
        self.pos.1 += 1;
        if let Some(b'\n') = self.stash.take() {
            self.pos.0 += 1;
            self.pos.1 = 1;
        }
    }

    fn rewind(&mut self) -> io::Result<()> {
        self.stash = None;
        self.pos = Pos(1, 1);
        self.inner.rewind()
    }
}
