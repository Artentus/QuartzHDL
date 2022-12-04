use crate::ast::*;
use crate::error::*;
use crate::{HashMap, HashSet, SharedString};

pub struct Scope<'p> {
    parent: Option<&'p Scope<'p>>,
    consts: HashSet<SharedString>,
    funcs: HashMap<SharedString, usize>,
    vars: HashSet<SharedString>,
}

impl<'p> Scope<'p> {
    pub fn empty() -> Self {
        Self {
            parent: None,
            consts: HashSet::default(),
            funcs: HashMap::default(),
            vars: HashSet::default(),
        }
    }

    pub fn new<'pp: 'p>(parent: &'p Scope<'pp>) -> Self {
        Self {
            parent: Some(parent),
            consts: HashSet::default(),
            funcs: HashMap::default(),
            vars: HashSet::default(),
        }
    }

    pub fn add_const(&mut self, name: impl Into<SharedString>) {
        self.consts.insert(name.into());
    }

    pub fn add_func(&mut self, name: impl Into<SharedString>, arg_count: usize) {
        self.funcs.insert(name.into(), arg_count);
    }

    pub fn add_var(&mut self, name: impl Into<SharedString>) {
        self.vars.insert(name.into());
    }

    pub fn contains_const_exclusive(&self, name: impl AsRef<str>) -> bool {
        self.consts.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_const_exclusive(name))
    }

    pub fn contains_func_exclusive(&self, name: impl AsRef<str>) -> Option<usize> {
        self.funcs.get(name.as_ref()).copied().or_else(|| {
            self.parent
                .and_then(|parent| parent.contains_func_exclusive(name))
        })
    }

    pub fn contains_var_exclusive(&self, name: impl AsRef<str>) -> bool {
        self.vars.contains(name.as_ref())
            || self
                .parent
                .map_or(false, |parent| parent.contains_var_exclusive(name))
    }

    pub fn contains_func<'err>(&self, name: &Ident) -> QuartzResult<'err, usize> {
        if let Some(arg_count) = self.contains_func_exclusive(name) {
            Ok(arg_count)
        } else if self.contains_const_exclusive(name) || self.contains_var_exclusive(name) {
            Err(QuartzError::InvalidFuncIdent { name: name.clone() })
        } else {
            Err(QuartzError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_const<'err>(&self, name: &Ident) -> QuartzResult<'err, ()> {
        if self.contains_const_exclusive(name) {
            Ok(())
        } else if self.contains_var_exclusive(name) {
            Err(QuartzError::ValueNotConst { name: name.clone() })
        } else if self.contains_func_exclusive(name).is_some() {
            Err(QuartzError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(QuartzError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_var<'err>(&self, name: &Ident) -> QuartzResult<'err, ()> {
        if self.contains_const_exclusive(name) || self.contains_var_exclusive(name) {
            Ok(())
        } else if self.contains_func_exclusive(name).is_some() {
            Err(QuartzError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(QuartzError::UndefinedIdent { name: name.clone() })
        }
    }

    pub fn contains_mut_var<'err>(&self, name: &Ident) -> QuartzResult<'err, ()> {
        if self.contains_var_exclusive(name) {
            Ok(())
        } else if self.contains_const_exclusive(name) {
            Err(QuartzError::TargetNotAssignable { name: name.clone() })
        } else if self.contains_func_exclusive(name).is_some() {
            Err(QuartzError::InvalidValueIdent { name: name.clone() })
        } else {
            Err(QuartzError::UndefinedIdent { name: name.clone() })
        }
    }
}
