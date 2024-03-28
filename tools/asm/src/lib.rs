use core::fmt;
use std::{
    fmt::{Display, Formatter},
    marker::PhantomData,
    ops::Range,
    slice, str,
};

pub struct SliceInterner<T> {
    pub storages: Vec<Vec<T>>,
}

impl<'a, T> SliceInterner<T>
where
    T: PartialEq + Eq + Clone,
{
    pub fn new() -> Self {
        Self {
            storages: Vec::new(),
        }
    }

    pub fn offset(&self, values: &[T]) -> Option<usize> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            if let Some(index) = storage.windows(values.len()).position(|win| win == values) {
                return Some(global_index + index);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn slice(&'a self, range: Range<usize>) -> Option<&'a [T]> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            let storage_range = global_index..(global_index + storage.len());
            if storage_range.contains(&range.start) && storage_range.contains(&(range.end - 1)) {
                return Some(&storage[range]);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn intern(&mut self, values: &[T]) -> &'a [T] {
        let mut has_space = None;
        for (i, storage) in self.storages.iter().enumerate() {
            // pre-check if we have space for the toks in case we have a cache miss
            if has_space.is_none() && ((storage.capacity() - storage.len()) >= values.len()) {
                has_space = Some(i);
            }
            if let Some(index) = storage.windows(values.len()).position(|win| win == values) {
                // SAFETY: the assumption is that we never re-allocate storages
                unsafe {
                    return slice::from_raw_parts(storage.as_ptr().add(index), values.len());
                }
            }
        }
        // cache miss, add to a storage if possible
        let storage = if let Some(index) = has_space {
            &mut self.storages[index]
        } else {
            self.storages
                .push(Vec::with_capacity(values.len().max(256)));
            self.storages.last_mut().unwrap()
        };
        let index = storage.len();
        storage.extend_from_slice(values);
        // SAFETY: the assumption is that we never re-allocate storages
        unsafe { slice::from_raw_parts(storage.as_ptr().add(index), values.len()) }
    }
}

pub struct StrInterner<'a> {
    pub storages: Vec<String>,
    pub marker: PhantomData<&'a ()>,
}

impl<'a> StrInterner<'a> {
    pub fn new() -> Self {
        Self {
            storages: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn offset(&self, string: &str) -> Option<usize> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            if let Some(index) = storage.find(string) {
                return Some(global_index + index);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn slice(&'a self, range: Range<usize>) -> Option<&'a str> {
        let mut global_index = 0;
        for storage in self.storages.iter() {
            let storage_range = global_index..(global_index + storage.len());
            if storage_range.contains(&range.start) && storage_range.contains(&(range.end - 1)) {
                return Some(&storage[range]);
            }
            global_index += storage.len();
        }
        None
    }

    pub fn intern(&mut self, string: &str) -> &'a str {
        let mut has_space = None;
        for (i, storage) in self.storages.iter().enumerate() {
            // pre-check if we have space for the string in case we have a cache miss
            if has_space.is_none() && ((storage.capacity() - storage.len()) >= string.len()) {
                has_space = Some(i);
            }
            if let Some(index) = storage.find(string) {
                // SAFETY: the assumption is that we never re-allocate storages
                unsafe {
                    return str::from_utf8_unchecked(slice::from_raw_parts(
                        storage.as_ptr().add(index),
                        string.len(),
                    ));
                }
            }
        }
        // cache miss, add to a storage if possible
        let storage = if let Some(index) = has_space {
            &mut self.storages[index]
        } else {
            self.storages.push(String::with_capacity(
                string.len().next_multiple_of(2).max(256),
            ));
            self.storages.last_mut().unwrap()
        };
        let index = storage.len();
        storage.push_str(string);
        // SAFETY: the assumption is that we never re-allocate storages
        unsafe {
            str::from_utf8_unchecked(slice::from_raw_parts(
                storage.as_ptr().add(index),
                string.len(),
            ))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Expr<'a> {
    Const(i32),
    Addr(&'a str, u32),
    List(&'a [ExprNode<'a>]),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprNode<'a> {
    Const(i32),
    Op(Op),
    Label(Label<'a>),
    Tag(Label<'a>, &'a str),
}

#[derive(Debug, Clone, Copy)]
pub enum RelocVal<'a> {
    Addr(&'a str, u32),
    HiAddr(&'a str, u32),
    List(&'a [ExprNode<'a>]),
    HiList(&'a [ExprNode<'a>]),
}

#[derive(Debug, Clone, Copy)]
pub struct Reloc<'a> {
    pub offset: usize,
    pub width: u8,
    pub value: RelocVal<'a>,
    pub unit: &'a str,
    pub file: &'a str,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct Section<'a> {
    pub name: &'a str,
    pub pc: u32,
    pub data: Vec<u8>,
    pub relocs: Vec<Reloc<'a>>,
}

impl<'a> Section<'a> {
    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            pc: 0,
            data: Vec::new(),
            relocs: Vec::new(),
        }
    }
}

pub struct SymFlags;

impl SymFlags {
    pub const NONE: u8 = 0;
    pub const EQU: u8 = 1 << 0;
}

#[derive(Debug)]
pub struct Sym<'a> {
    pub label: Label<'a>,
    pub value: Expr<'a>,
    pub unit: &'a str,
    pub section: &'a str,
    pub file: &'a str,
    pub pos: Pos,
    pub flags: u8,
}

impl<'a> Sym<'a> {
    pub fn new(
        label: Label<'a>,
        value: Expr<'a>,
        unit: &'a str,
        section: &'a str,
        file: &'a str,
        pos: Pos,
        flags: u8,
    ) -> Self {
        Self {
            label,
            value,
            unit,
            section,
            file,
            pos,
            flags,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label<'a> {
    pub scope: Option<&'a str>,
    pub string: &'a str,
}

impl<'a> Label<'a> {
    pub fn new(scope: Option<&'a str>, string: &'a str) -> Self {
        Self { scope, string }
    }
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(scope) = self.scope {
            write!(f, "{scope}{}", self.string)
        } else {
            write!(f, "{}", self.string)
        }
    }
}

impl<'a, T: AsRef<str>> PartialEq<T> for Label<'a> {
    fn eq(&self, other: &T) -> bool {
        let other = other.as_ref();
        if let Some(scope) = self.scope {
            ((scope.len() + self.string.len()) == other.len())
                && other.starts_with(scope)
                && other.ends_with(self.string)
        } else {
            other == self.string
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Binary(Tok),
    Unary(Tok),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Tok(pub u8);

impl Tok {
    pub const NEWLINE: Self = Self(b'\n');
    pub const MODULUS: Self = Self(b'%');
    pub const SOLIDUS: Self = Self(b'/');
    pub const STAR: Self = Self(b'*');
    pub const PLUS: Self = Self(b'+');
    pub const MINUS: Self = Self(b'-');
    pub const LT: Self = Self(b'<');
    pub const GT: Self = Self(b'>');
    pub const AMP: Self = Self(b'&');
    pub const CARET: Self = Self(b'^');
    pub const PIPE: Self = Self(b'|');
    pub const LPAREN: Self = Self(b'(');
    pub const RPAREN: Self = Self(b')');
    pub const LBRACKET: Self = Self(b'[');
    pub const RBRACKET: Self = Self(b']');
    pub const BANG: Self = Self(b'!');
    pub const TILDE: Self = Self(b'~');
    pub const HASH: Self = Self(b'#');
    pub const COMMA: Self = Self(b',');
    pub const COLON: Self = Self(b':');
    pub const EQU: Self = Self(b'=');

    pub const A: Self = Self(b'A');
    pub const B: Self = Self(b'B');
    pub const C: Self = Self(b'C');
    pub const D: Self = Self(b'D');
    pub const E: Self = Self(b'E');
    pub const H: Self = Self(b'H');
    pub const L: Self = Self(b'L');
    pub const Z: Self = Self(b'Z');

    pub const EOF: Self = Self(0x80);
    pub const IDENT: Self = Self(0x81);
    pub const NUM: Self = Self(0x82);
    pub const STR: Self = Self(0x83);
    pub const ARG: Self = Self(0x84);

    pub const ASL: Self = Self(0x85); // <<
    pub const ASR: Self = Self(0x86); // >>
    pub const LSR: Self = Self(0x87); // ~>
    pub const LTE: Self = Self(0x88); // <=
    pub const GTE: Self = Self(0x89); // >=
    pub const EQ: Self = Self(0x8A); // ==
    pub const NEQ: Self = Self(0x8B); // !=
    pub const AND: Self = Self(0x8C); // &&
    pub const OR: Self = Self(0x8D); // ||

    pub const AF: Self = Self(0x8E); // AF
    pub const BC: Self = Self(0x8F); // BC
    pub const DE: Self = Self(0x90); // DE
    pub const HL: Self = Self(0x91); // HL
    pub const SP: Self = Self(0x92); // SP
    pub const NC: Self = Self(0x93); // NC
    pub const NZ: Self = Self(0x94); // NZ
}

#[derive(Debug, Clone, Copy)]
pub struct Pos(pub usize, pub usize);
