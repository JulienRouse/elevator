# Elevator

very early work in progress.

Simple simulation for elevator strategy.

## Usage

after loading it:
```
*(el::setup)
*(el::setup-thread)
```
should launch two thread, one that generate call, 
and one that operate an elevator.

## Installation

git clone https://github.com/JulienRouse/elevator.git

To load it, if you have quicklisp, you can clone it in quicklisp/local-projects and then
```
*(ql:register-local-project)
*(ql:quickload :elevator)
```
## Author

* Julien Rousé (julien.rouse@gmail.com)

## Copyright

Copyright (c) 2015 Julien Rousé (julien.rouse@gmail.com)

## License

Licensed under the WTFPL License.
