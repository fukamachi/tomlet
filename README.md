# tomlet

A TOML v1.0.0 compliant parser for Common Lisp.

## Quick Start

```lisp
(ql:quickload :tomlet)

;; Parse a TOML string
(tomlet:parse "
[server]
host = \"localhost\"
port = 8080
enabled = true
")
; => #<HASH-TABLE :TEST EQUAL :COUNT 1>

;; Parse a TOML file
(tomlet:parse-file "config.toml")
; => #<HASH-TABLE :TEST EQUAL :COUNT N>

;; Access parsed data
(let ((config (tomlet:parse-file "config.toml")))
  (let ((server (gethash "server" config)))
    (format t "Host: ~A~%" (gethash "host" server))
    (format t "Port: ~D~%" (gethash "port" server))))
```

## API Reference

### Functions

#### parse (string)

Parse a TOML string and return a hash table.

```lisp
(tomlet:parse "key = \"value\"")
; => Hash table with "key" => "value"
```

#### parse-file (filename)

Parse a TOML file and return a hash table.

```lisp
(tomlet:parse-file #p"config.toml")
```

### Data Types

TOML values map to Common Lisp types as follows:

| TOML Type | Common Lisp Type | Example |
|-----------|------------------|---------|
| String | `string` | `"hello"` |
| Integer | `integer` | `42` |
| Float | `double-float` | `3.14d0` |
| Boolean | `boolean` (`t`/`nil`) | `t` |
| Array | `vector` | `#(1 2 3)` |
| Table | `hash-table` | `#<HASH-TABLE>` |
| Inline Table | `hash-table` | `#<HASH-TABLE>` |
| Date-Time | See below | |

#### Date and Time Types

tomlet provides four distinct datetime types per TOML v1.0.0 spec:

**Offset Date-Time** (with timezone)
```lisp
;; 1979-05-27T07:32:00Z
(tomlet:make-offset-datetime
  :year 1979 :month 5 :day 27
  :hour 7 :minute 32 :second 0
  :nanosecond 0 :offset 0)
```

**Local Date-Time** (without timezone)
```lisp
;; 1979-05-27T07:32:00
(tomlet:make-local-datetime
  :year 1979 :month 5 :day 27
  :hour 7 :minute 32 :second 0
  :nanosecond 0)
```

**Local Date** (date only)
```lisp
;; 1979-05-27
(tomlet:make-local-date
  :year 1979 :month 5 :day 27)
```

**Local Time** (time only)
```lisp
;; 07:32:00
(tomlet:make-local-time
  :hour 7 :minute 32 :second 0
  :nanosecond 0)
```

Each type has corresponding constructor, predicate, and accessor functions:
- `make-local-date`, `local-date-p`, `local-date-year`, etc.
- `make-local-time`, `local-time-p`, `local-time-hour`, etc.
- `make-local-datetime`, `local-datetime-p`, `local-datetime-year`, etc.
- `make-offset-datetime`, `offset-datetime-p`, `offset-datetime-year`, `offset-datetime-offset`, etc.

### Error Conditions

#### `toml-error`

Base error condition for all TOML-related errors.

**Accessors:**
- `toml-error-message` - Error message string

#### `toml-parse-error`

Signaled when parsing fails due to invalid TOML syntax.

**Accessors:**
- `toml-error-message` - Error message string
- `toml-parse-error-line` - Line number where error occurred (or `nil`)
- `toml-parse-error-column` - Column number where error occurred (or `nil`)

**Example:**

```lisp
(handler-case
    (tomlet:parse "invalid toml = ")
  (tomlet:toml-parse-error (e)
    (format t "Parse error at line ~D, column ~D: ~A~%"
            (tomlet:toml-parse-error-line e)
            (tomlet:toml-parse-error-column e)
            (tomlet:toml-error-message e))))
```

## Supported TOML Features

### ✅ Fully Supported

- **Comments** - `# This is a comment`
- **Key-Value Pairs** - `key = "value"`
- **Strings**
  - Basic strings: `"hello"`
  - Multi-line basic strings: `"""multi\nline"""`
  - Literal strings: `'C:\Users\name'`
  - Multi-line literal strings: `'''no\escape'''`
- **Integers**
  - Decimal: `42`, `+17`, `-5`
  - Hex: `0xDEADBEEF`
  - Octal: `0o755`
  - Binary: `0b11010110`
  - With underscores: `1_000_000`
- **Floats**
  - Standard: `3.14`, `1.5e10`
  - Special values: `inf`, `-inf`, `nan`, `+inf`, `+nan`, `-nan`
- **Booleans** - `true`, `false`
- **Date and Time**
  - Offset date-time: `1979-05-27T07:32:00Z`
  - Local date-time: `1979-05-27T07:32:00`
  - Local date: `1979-05-27`
  - Local time: `07:32:00`
- **Arrays** - `[1, 2, 3]`
  - Heterogeneous: `[1, "two", 3.0]`
  - Nested: `[[1, 2], [3, 4]]`
  - Multi-line with trailing commas
- **Tables** - `[table]`
- **Inline Tables** - `{x = 1, y = 2}`
- **Array of Tables** - `[[products]]`
- **Dotted Keys** - `a.b.c = "value"`

All TOML v1.0.0 features are fully supported with no known limitations.

## Examples

### Configuration File

**config.toml:**
```toml
# Database configuration
[database]
host = "localhost"
port = 5432
enabled = true
max_connections = 100

[database.credentials]
username = "admin"
password = "secret"

# Server configuration
[server]
bind = "0.0.0.0:8080"
workers = 4
timeout = 30.0

# Features
features = [
    "auth",
    "logging",
    "metrics"
]
```

**Loading:**
```lisp
(defun load-config (path)
  (let ((config (tomlet:parse-file path)))
    (let ((db (gethash "database" config))
          (server (gethash "server" config)))
      (list :db-host (gethash "host" db)
            :db-port (gethash "port" db)
            :server-bind (gethash "bind" server)
            :features (gethash "features" config)))))

(load-config "config.toml")
; => (:DB-HOST "localhost" :DB-PORT 5432 :SERVER-BIND "0.0.0.0:8080"
;     :FEATURES #("auth" "logging" "metrics"))
```

### Nested Tables

```lisp
(tomlet:parse "
[fruit]
name = \"apple\"

[fruit.physical]
color = \"red\"
shape = \"round\"

[fruit.variety]
name = \"red delicious\"
")

;; Access nested tables:
(let* ((data (tomlet:parse ...))
       (fruit (gethash "fruit" data))
       (physical (gethash "physical" fruit)))
  (gethash "color" physical))
; => "red"
```

### Array of Tables

```lisp
(tomlet:parse "
[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]
name = \"Nail\"
sku = 284758393
")

;; Access array of tables:
(let* ((data (tomlet:parse ...))
       (products (gethash "products" data)))
  (loop for product across products
        collect (gethash "name" product)))
; => ("Hammer" "Nail")
```

### Date and Time

```lisp
(let* ((data (tomlet:parse "
offset_dt = 1979-05-27T07:32:00Z
local_dt = 1979-05-27T07:32:00
date = 1979-05-27
time = 07:32:00
"))
       (offset-dt (gethash "offset_dt" data))
       (local-dt (gethash "local_dt" data))
       (date (gethash "date" data))
       (time (gethash "time" data)))

  (list :offset-year (tomlet:offset-datetime-year offset-dt)
        :local-year (tomlet:local-datetime-year local-dt)
        :date-day (tomlet:local-date-day date)
        :time-hour (tomlet:local-time-hour time)))
; => (:OFFSET-YEAR 1979 :LOCAL-YEAR 1979 :DATE-DAY 27 :TIME-HOUR 7)
```

## Compliance & Test Coverage

- **Official TOML v1.0.0 test suite:** **100% passing (734/734 tests)**
  - 205 valid tests: 205 passing ✓
  - 529 invalid tests: 529 passing ✓
  - Full compliance with TOML v1.0.0 specification

## License

MIT License - see [LICENSE](./LICENSE) file for details.

## See Also

- **TOML Spec:** https://toml.io/
- **Official Test Suite:** https://github.com/toml-lang/toml-test
