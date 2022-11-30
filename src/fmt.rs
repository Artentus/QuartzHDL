use std::fmt;

pub struct ScopedFormatter<'a, 'f: 'a> {
    inner: &'a mut fmt::Formatter<'f>,
    level: usize,
    at_line_start: bool,
}

impl<'a, 'f: 'a> ScopedFormatter<'a, 'f> {
    #[inline]
    pub fn enter_scope(&mut self) {
        self.level += 1;
    }

    #[inline]
    pub fn exit_scope(&mut self) {
        debug_assert!(self.level > 0);
        self.level -= 1;
    }
}

impl<'a, 'f: 'a> fmt::Write for ScopedFormatter<'a, 'f> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if !s.is_empty() {
            let indent_width = self.level * 4;

            for (i, line) in s.lines().enumerate() {
                if i > 0 {
                    writeln!(self.inner)?;
                }

                if !line.is_empty() {
                    if (i > 0) || self.at_line_start {
                        write!(self.inner, "{:width$}", "", width = indent_width)?;
                    }

                    self.inner.write_str(line)?;
                }
            }

            self.at_line_start = s.ends_with('\n');
            if self.at_line_start {
                writeln!(self.inner)?;
            }
        }

        Ok(())
    }
}

pub trait ToScoped<'f> {
    fn to_scoped<'a>(&'a mut self) -> ScopedFormatter<'a, 'f>
    where
        'f: 'a;
}

impl<'f> ToScoped<'f> for fmt::Formatter<'f> {
    fn to_scoped<'a>(&'a mut self) -> ScopedFormatter<'a, 'f>
    where
        'f: 'a,
    {
        ScopedFormatter {
            inner: self,
            level: 0,
            at_line_start: true,
        }
    }
}

pub trait DisplayScoped {
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> fmt::Result;
}

impl DisplayScoped for crate::SharedString {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> fmt::Result {
        use std::fmt::Write;
        write!(f, "{}", self)
    }
}

impl DisplayScoped for i64 {
    #[inline]
    fn fmt(&self, f: &mut ScopedFormatter<'_, '_>) -> fmt::Result {
        use std::fmt::Write;
        write!(f, "{}", self)
    }
}

#[macro_export]
macro_rules! default_display_impl {
    ($t:ty) => {
        impl std::fmt::Display for $t {
            #[inline]
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                use $crate::fmt::ToScoped;
                $crate::fmt::DisplayScoped::fmt(self, &mut f.to_scoped())
            }
        }
    };
}
