# muscala
A music theory library implemented in Scala

## Background

This is a music theory library inspired by Python's 
[mingus](https://github.com/bspaans/python-mingus), which aims to take musical 
concepts such as notes and keys, and represent them in a Scala-oriented library.
Unlike other music theory libraries, this library attempts to represent
these concepts in a Scala-esque manner by using both object-oriented and
functional programming concepts.

This project is also a hobby of mine as I learn more about music theory! I will
occasionally visit this library and add new concepts that I learn.

## Contributing
Feel free to open up a pull request, and I'll happily review it! I do ask that
any PRs introducing new code contains documentations for the public functions
at the bare minimum, but other than that, we can have a discussion in the
PR section!

### Testing
This project emphasizes [property testing](https://www.scalatest.org/user_guide/property_based_testing) 
as the primary means of testing code components. While I will accept the 
standard JUnit-style test (ScalaTest does support a variety of test suite 
styles), I prefer tested code as property checks. There are occasions where
table-driven tests may be needed, and those are totally fine!

## License
This project is under the MIT License, please look at the license for more
details on what rights you have to this project.
