module package
import locktypes

version : Version
version = MkVersion 1 1 1

dependancies : List (Dependancy, Version)
dependancies = [
    (Local "/Users/jamie/Documents/Uni/fourth_year/diss/ipm", MkVersion 1 0 0),
    (Url "https://www.github.com/Jamster7777/test-idris-package"), MkVersion 0 1 10)
]

