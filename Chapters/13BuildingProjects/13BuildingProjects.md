# 13 Building Projects
Haskell [Cabal](https://cabal.readthedocs.io/en/stable/) (Common Architecture for Building Applications and Libraries) is a package manager. A package is a program that may have dependencies.

**Stack** is a program for developing Haskell projects. It is built on top of Cabal. The command `stack build` builds a project and `stack setup` […]. `stack ghci` starts GHCi in the context of a program, where functions can be executed. `stack new <project-name> simple` creates a new project using the [template](https://github.com/commercialhaskell/stack-templates) “simple”.

The **.cabal** file (located in a project’s root folder) contains information about the project. For example whether it is a library or an executable.

```Text
library | executable program-name
  hs-source-dirs:      src
 [exposed-modules:     Module1, Module2]  -- for libraries
 [main-is:             Main.hs]  -- for executables
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
```

However, with Stack projects, there is also a `stack.yaml` file, which contains dependencies. It should be preferred over the Cabal file.

A program can be started with `stack exec <program-name>` from every directory. However, the program executable is only present if the `.cabal` file that was built before contains the line `executable <program-name>` and if the program was built.

By default a module **exports** all its content. This can be changed by adding a list of exported items:

```haskell
module ModuleName
  (function1, constant1)
  where

-- implementation of function1, constant 1, and possibly more
```

The importing module can also choose what to import. This is done similarly, through a list of items, e.g. `import Data.Bool (bool)`. The other way around, certain things can be excluded from an import: `import Database.SQLite.Simple hiding (close)`.

**Qualified imports** persist the fully qualified name of the imported items. That means with `import qualified Data.Bool` the function `bool` is only accessible through `Data.Bool.bool`.

With an **alias**, e.g. `import qualified Data.Bool as B`, `bool` is accessible through `B.bool`.

In GHCi the `:browse <Module>` command lists all items exported by a module. `Prelude` can be disabled with the command `stack ghci --ghci-options -XNoImplicitPrelude`.

## 13.1 Read CSV File
Example snippet that reads the file “data.csv”:

```haskell
module CSVReader (readCsv) where

import Data.List.Split (splitOn)

readCsv :: IO [[String]]
readCsv = do
  raw <- readFile "data.csv"
  return $ parseCsv raw
  where
    parseCsv :: String -> [[String]]
    parseCsv s = map (splitOn ",") (lines s)
```