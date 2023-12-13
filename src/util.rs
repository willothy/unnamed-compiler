use chumsky::{primitive::just, text::whitespace, IterParser, Parser};

pub fn comma_separated<'a, P: 'a + Parser<'a, &'a str, T>, T: 'a>(
    parser: P,
) -> impl Parser<'a, &'a str, Vec<T>> + Clone {
    parser
        .separated_by(just(",").then_ignore(whitespace()))
        .collect::<Vec<_>>()
        .boxed()
}
