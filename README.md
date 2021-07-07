# jgrep

## Pre-requesites

Install `ghcup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Install `ghc`:

```
ghcup install ghc 8.10.4
ghcup set ghc 8.10.4
```

Install `cabal`:

```
ghcup install cabal 3.4.0.0
ghcup set cabal 3.4.0.0
```

Build with:

```
cabal build all
```

Test with:

```
cabal test all
```

## Design

### Database data structure

The database data structures can be found in the `JGrep.Type` module.

The database is composed of multiple "tables" indexed by typename (eg. "users").

```haskell
newtype Database = Database
  { tables :: HashMap TypeName Table
  }
```

A table is composed of the data for a particular type (`records`) along side all the indexes (`matches`).

```haskell
data Table = Table
  { records :: Vector Value
  , indexes :: Indexes
  }
```

There are two kinds of indexes.  One to find exact matches (`exact`) and one to find element membership of (`arrays`):

```haskell
data Indexes = Indexes
  { exact :: HashMap (Key, Value) (HashSet Int)
  , contains :: HashMap (Key, Value) (HashSet Int)
  }
```

The type of an index is a `HashMap (Key, Value) (HashSet Int)`.

In the `exact` match case, the key and value represents the exact match and the hash set is the set of all exact matches.

In the `contains` match case, the key and value represents the element membership and the hash set is the set of all membership
matches.

## Pros

The database as designed is easily built.  For example `Indexes` is a monoid which means I can map records to singleton indexes
(an index that only indexes one match for one record) and use folds and monoid appends to combine these singleton indexes into
a full index.  This makes index building easy.

It is also easy to write logical filters that use `and` and `or` which are intersections and unions of the result set
respectively.

## Cons

The results are unordered.

Representation of large result sets that can easily be produced by negative matches (ie. `key != "value"`) is memory intensive.

Requires one two indexes per field.

## Completed

Loading of data.  In-code queries.  Some basic tests.

## Missing features

The ability to do negative matches.

The ability to link between types (for example to see associated entities of the result set).

Proper command line interface for queries.

## Possible improvements

Use a finger tree instead of a hash set in indexes.  This would allow result sets to remain in record order by default.  Possibly better data sharing.

Use ordered map based on finger trees instead of hashmap in indexes.  Alternatively, use a sorted vector.  This opens the way for range searches.

Use a succinct-data structure JSON parser like `hw-json` which will reduce the memory usage of storing the JSON in
memory by about an order of magnitude.

Store the indexes on disk to reduce memory usage.

Use z-ordering to combine all indexes for a given type into one.  This will minimise the  This is an ambitious project requiring multiple weeks of full time work.

# Project structure

## Modules

* `JGrep.Database` code for loading the database
* `JGrep.Type` data types include types for the database
* `JGrep.Schema` code for describing the schema (basically where the files are and the name for the type of the records therein)
* `JGrep.Query` code for evaluating queries
* `JGrep.CoreSec` some unit tests
