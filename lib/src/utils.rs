use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Result, Token,
};

#[derive(Debug)]
pub struct Expressions(pub Vec<Expr>);

impl Parse for Expressions {
    fn parse(input: ParseStream) -> Result<Self> {
        let expressions: Vec<_> = input
            .parse_terminated(Expr::parse, Token![,])?
            .into_iter()
            .collect();
        Ok(Expressions(expressions))
    }
}

pub fn flatten_punctuated<T, P>(punctuated: &Punctuated<T, P>) -> Vec<T>
where
    T: Clone,
{
    punctuated.iter().map(|arg| arg.clone()).collect()
}
