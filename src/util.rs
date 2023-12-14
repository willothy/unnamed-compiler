use chumsky::{error::Rich, extra, primitive::just, text::whitespace, IterParser, Parser};

pub fn comma_separated<'a, P: 'a + Parser<'a, &'a str, T, extra::Err<Rich<'a, char>>>, T: 'a>(
    parser: P,
) -> impl Parser<'a, &'a str, Vec<T>, extra::Err<Rich<'a, char>>> + Clone {
    parser
        .separated_by(just(",").then_ignore(whitespace()))
        .collect::<Vec<_>>()
        .boxed()
}
