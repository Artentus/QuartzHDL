use crate::fmt::{DisplayScoped, ScopedFormatter};
use crate::{default_display_impl, SharedString};
use langbox::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PunctKind {
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `{`
    OpenCurl,
    /// `}`
    CloseCurl,
    /// `,`
    Comma,
    /// `..`
    DoublePeriod,
    /// `.`
    Period,
    /// `::`
    DoubleColon,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `@`
    AtSymbol,
    /// `#`
    Hash,
    /// `=>`
    FatRightArrow,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<=$`
    Slte,
    /// `>=$`
    Sgte,
    /// `<=`
    Lte,
    /// `>=`
    Gte,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    RemAssign,
    /// `&=`
    AndAssign,
    /// `|=`
    OrAssign,
    /// `^=`
    XorAssign,
    /// `<<=`
    ShlAssign,
    /// `>>>=`
    AsrAssign,
    /// `>>=`
    LsrAssign,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `&`
    And,
    /// `|`
    Or,
    /// `^`
    Xor,
    /// `!`
    Not,
    /// `<<`
    Shl,
    /// `>>>`
    Asr,
    /// `>>`
    Lsr,
    /// `<$`
    Slt,
    /// `>$`
    Sgt,
    /// `<`
    Lt,
    /// `>`
    Gt,
}

impl DisplayScoped for PunctKind {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> std::fmt::Result {
        use std::fmt::Write;

        write!(
            f,
            "{}",
            match self {
                PunctKind::OpenParen => "(",
                PunctKind::CloseParen => ")",
                PunctKind::OpenBracket => "[",
                PunctKind::CloseBracket => "]",
                PunctKind::OpenCurl => "{",
                PunctKind::CloseCurl => "}",
                PunctKind::Comma => ",",
                PunctKind::DoublePeriod => "..",
                PunctKind::Period => ".",
                PunctKind::DoubleColon => "::",
                PunctKind::Colon => ":",
                PunctKind::Semicolon => ";",
                PunctKind::AtSymbol => "@",
                PunctKind::Hash => "#",
                PunctKind::FatRightArrow => "=>",
                PunctKind::Eq => "==",
                PunctKind::Ne => "!=",
                PunctKind::Slte => "<=$",
                PunctKind::Sgte => ">=$",
                PunctKind::Lte => "<=",
                PunctKind::Gte => ">=",
                PunctKind::Assign => "=",
                PunctKind::AddAssign => "+=",
                PunctKind::SubAssign => "-=",
                PunctKind::MulAssign => "*=",
                PunctKind::DivAssign => "/=",
                PunctKind::RemAssign => "%=",
                PunctKind::AndAssign => "&=",
                PunctKind::OrAssign => "|=",
                PunctKind::XorAssign => "^=",
                PunctKind::ShlAssign => "<<=",
                PunctKind::AsrAssign => ">>>=",
                PunctKind::LsrAssign => ">>=",
                PunctKind::Add => "+",
                PunctKind::Sub => "-",
                PunctKind::Mul => "*",
                PunctKind::Div => "/",
                PunctKind::Rem => "%",
                PunctKind::And => "&",
                PunctKind::Or => "|",
                PunctKind::Xor => "^",
                PunctKind::Not => "!",
                PunctKind::Shl => "<<",
                PunctKind::Asr => ">>>",
                PunctKind::Lsr => ">>",
                PunctKind::Slt => "<$",
                PunctKind::Sgt => ">$",
                PunctKind::Lt => "<",
                PunctKind::Gt => ">",
            }
        )
    }
}

default_display_impl!(PunctKind);

impl From<PunctKind> for [PunctKind; 1] {
    #[inline]
    fn from(value: PunctKind) -> Self {
        [value]
    }
}

#[derive(Debug, Clone)]
pub enum QuartzToken {
    Comment(bool),
    Punct(PunctKind),
    Ident(SharedString),
    Literal(i64),
    String(SharedString),
    InvalidIdent(SharedString),
    InvalidLiteral(SharedString),
    InvalidString {
        string: SharedString,
        invalid_escape_offsets: Vec<usize>,
        is_open: bool,
    },
    InvalidChar(char),
}

#[rustfmt::skip]
const PUNCTUATION_MAP: &[(&str, PunctKind)] = &[
    ("("   , PunctKind::OpenParen    ),
    (")"   , PunctKind::CloseParen   ),
    ("["   , PunctKind::OpenBracket  ),
    ("]"   , PunctKind::CloseBracket ),
    ("{"   , PunctKind::OpenCurl     ),
    ("}"   , PunctKind::CloseCurl    ),
    (","   , PunctKind::Comma        ),
    (".."  , PunctKind::DoublePeriod ),
    ("."   , PunctKind::Period       ),
    ("::"  , PunctKind::DoubleColon  ),
    (":"   , PunctKind::Colon        ),
    (";"   , PunctKind::Semicolon    ),
    ("@"   , PunctKind::AtSymbol     ),
    ("#"   , PunctKind::Hash         ),
    ("=>"  , PunctKind::FatRightArrow),
    ("=="  , PunctKind::Eq           ),
    ("!="  , PunctKind::Ne           ),
    ("<=$" , PunctKind::Slte         ),
    (">=$" , PunctKind::Sgte         ),
    ("<="  , PunctKind::Lte          ),
    (">="  , PunctKind::Gte          ),
    ("="   , PunctKind::Assign       ),
    ("+="  , PunctKind::AddAssign    ),
    ("-="  , PunctKind::SubAssign    ),
    ("*="  , PunctKind::MulAssign    ),
    ("/="  , PunctKind::DivAssign    ),
    ("%="  , PunctKind::RemAssign    ),
    ("&="  , PunctKind::AndAssign    ),
    ("|="  , PunctKind::OrAssign     ),
    ("^="  , PunctKind::XorAssign    ),
    ("<<=" , PunctKind::ShlAssign    ),
    (">>>=", PunctKind::AsrAssign    ),
    (">>=" , PunctKind::LsrAssign    ),
    ("+"   , PunctKind::Add          ),
    ("-"   , PunctKind::Sub          ),
    ("*"   , PunctKind::Mul          ),
    ("/"   , PunctKind::Div          ),
    ("%"   , PunctKind::Rem          ),
    ("&"   , PunctKind::And          ),
    ("|"   , PunctKind::Or           ),
    ("^"   , PunctKind::Xor          ),
    ("!"   , PunctKind::Not          ),
    ("<<"  , PunctKind::Shl          ),
    (">>>" , PunctKind::Asr          ),
    (">>"  , PunctKind::Lsr          ),
    ("<$"  , PunctKind::Slt          ),
    (">$"  , PunctKind::Sgt          ),
    ("<"   , PunctKind::Lt           ),
    (">"   , PunctKind::Gt           ),
];

fn parse_comment(text: &str) -> Option<ReadTokenResult<QuartzToken>> {
    if text.starts_with("//") {
        let end_index = text
            .char_indices()
            .take_while(|(_, c)| *c != '\n')
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap(); // Ok because we know 'text' starts with a valid char

        Some(ReadTokenResult {
            token: QuartzToken::Comment(false),
            consumed_bytes: end_index,
        })
    } else if let Some(text) = text.strip_prefix("/*") {
        if let Some(len) = text.find("*/") {
            Some(ReadTokenResult {
                token: QuartzToken::Comment(false),
                consumed_bytes: len + "/**/".len(),
            })
        } else {
            Some(ReadTokenResult {
                token: QuartzToken::Comment(true), // This is an error state
                consumed_bytes: text.len(),
            })
        }
    } else {
        None
    }
}

fn parse_punctuation(text: &str) -> Option<ReadTokenResult<QuartzToken>> {
    for (pat, punct) in PUNCTUATION_MAP.iter() {
        if text.starts_with(pat) {
            return Some(ReadTokenResult {
                token: QuartzToken::Punct(*punct),
                consumed_bytes: pat.len(),
            });
        }
    }

    None
}

fn parse_identifier(text: &str) -> Option<ReadTokenResult<QuartzToken>> {
    if text.starts_with(|c: char| c.is_alphabetic() | (c == '_')) {
        let end_index = text
            .char_indices()
            .take_while(|(_, c)| c.is_alphanumeric() | (*c == '_'))
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap(); // Ok because we know 'text' starts with a valid char

        let ident: SharedString = text[..end_index].into();
        let token = if ident.contains(|c: char| !c.is_ascii_alphanumeric() & (c != '_')) {
            QuartzToken::InvalidIdent(ident)
        } else {
            QuartzToken::Ident(ident)
        };

        Some(ReadTokenResult {
            token,
            consumed_bytes: end_index,
        })
    } else {
        None
    }
}

fn parse_literal(text: &str) -> Option<ReadTokenResult<QuartzToken>> {
    if text.starts_with(|c: char| c.is_ascii_digit()) {
        let end_index = text
            .char_indices()
            .take_while(|(_, c)| c.is_alphanumeric() | (*c == '_'))
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap(); // Ok because we know 'text' starts with a valid char

        let raw_literal = &text[..end_index];
        let (literal, radix) = if let Some(raw_literal) = raw_literal.strip_prefix("0x") {
            (raw_literal.replace('_', ""), 16)
        } else if let Some(raw_literal) = raw_literal.strip_prefix("0o") {
            (raw_literal.replace('_', ""), 8)
        } else if let Some(raw_literal) = raw_literal.strip_prefix("0b") {
            (raw_literal.replace('_', ""), 2)
        } else {
            (raw_literal.replace('_', ""), 10)
        };

        // We parse as u64 because literal tokens are always unsigned but we still want to support the whole 64bit range
        let token = match u64::from_str_radix(&literal, radix) {
            Ok(value) => QuartzToken::Literal(value as i64),
            Err(_) => QuartzToken::InvalidLiteral(raw_literal.into()),
        };

        Some(ReadTokenResult {
            token,
            consumed_bytes: end_index,
        })
    } else {
        None
    }
}

fn parse_string(text: &str) -> Option<ReadTokenResult<QuartzToken>> {
    let text = text.strip_prefix('"')?;
    let mut chars = text.char_indices();

    let mut invalid_escape_offsets = Vec::new();
    let mut string = String::new();
    loop {
        if let Some((i, c)) = chars.next() {
            if c == '"' {
                return if invalid_escape_offsets.is_empty() {
                    Some(ReadTokenResult {
                        token: QuartzToken::String(string.into()),
                        consumed_bytes: i + ("\"".len() * 2),
                    })
                } else {
                    Some(ReadTokenResult {
                        token: QuartzToken::InvalidString {
                            string: string.into(),
                            invalid_escape_offsets,
                            is_open: false,
                        },
                        consumed_bytes: i + ("\"".len() * 2),
                    })
                };
            } else if c == '\\' {
                string.push('\\');

                if let Some((_, c)) = chars.next() {
                    if matches!(c, '"' | '\\' | 'r' | 'n' | 't' | 'b' | '0') {
                        string.push(c);
                    } else {
                        invalid_escape_offsets.push(i + "\"".len());
                    }
                } else {
                    return Some(ReadTokenResult {
                        token: QuartzToken::InvalidString {
                            string: string.into(),
                            invalid_escape_offsets,
                            is_open: true,
                        },
                        consumed_bytes: i + "\"".len() + "\\".len(),
                    });
                }
            } else if c == '\n' {
                return Some(ReadTokenResult {
                    token: QuartzToken::InvalidString {
                        string: string.into(),
                        invalid_escape_offsets,
                        is_open: true,
                    },
                    consumed_bytes: i + "\"".len() + "\n".len(),
                });
            } else {
                string.push(c);
            }
        } else {
            return Some(ReadTokenResult {
                token: QuartzToken::InvalidString {
                    string: string.into(),
                    invalid_escape_offsets,
                    is_open: true,
                },
                consumed_bytes: text.len(),
            });
        }
    }
}

pub struct QuartzTokenReader;
impl TokenReader for QuartzTokenReader {
    type TokenKind = QuartzToken;

    fn read_token(text: &str) -> ReadTokenResult<Self::TokenKind> {
        if let Some(result) = parse_comment(text) {
            return result;
        }

        if let Some(result) = parse_punctuation(text) {
            return result;
        }

        if let Some(result) = parse_identifier(text) {
            return result;
        }

        if let Some(result) = parse_literal(text) {
            return result;
        }

        if let Some(result) = parse_string(text) {
            return result;
        }

        let first_char = text.chars().next().unwrap();
        ReadTokenResult {
            token: QuartzToken::InvalidChar(first_char),
            consumed_bytes: first_char.len_utf8(),
        }
    }
}

pub type QuartzLexer<'a> = Lexer<'a, QuartzTokenReader, whitespace_mode::Remove>;

#[derive(Debug)]
pub enum QuartzLexerError {
    OpenBlockComment {
        span: TextSpan,
    },
    InvalidIdent {
        ident: SharedString,
        span: TextSpan,
    },
    InvalidLiteral {
        literal: SharedString,
        span: TextSpan,
    },
    InvalidString {
        string: SharedString,
        span: TextSpan,
        invalid_escape_offsets: Vec<usize>,
        is_open: bool,
    },
    InvalidChar {
        char: char,
        span: TextSpan,
    },
}

pub fn get_token_error(token: &Token<QuartzToken>) -> Option<QuartzLexerError> {
    match &token.kind {
        QuartzToken::Comment(true) => Some(QuartzLexerError::OpenBlockComment { span: token.span }),
        QuartzToken::InvalidIdent(ident) => Some(QuartzLexerError::InvalidIdent {
            ident: SharedString::clone(ident),
            span: token.span,
        }),
        QuartzToken::InvalidLiteral(literal) => Some(QuartzLexerError::InvalidLiteral {
            literal: SharedString::clone(literal),
            span: token.span,
        }),
        QuartzToken::InvalidString {
            string,
            invalid_escape_offsets,
            is_open,
        } => Some(QuartzLexerError::InvalidString {
            string: SharedString::clone(string),
            span: token.span,
            invalid_escape_offsets: invalid_escape_offsets.clone(),
            is_open: *is_open,
        }),
        QuartzToken::InvalidChar(char) => Some(QuartzLexerError::InvalidChar {
            char: *char,
            span: token.span,
        }),
        _ => None,
    }
}
