# Validate a Sub Portfolio Configuration

Validates a `sub_port_config` (or any class extending it, such as
`mmaf_sub_port_config`). It is the single validity contract shared by
every layered (`*af`) portfolio construction method, so that a
sub-portfolio is specified the same way regardless of which method
builds it.

## Usage

``` r
validate_sub_port_config(object)
```

## Arguments

- object:

  An object of class `sub_port_config` or of a class extending it.

## Value

`TRUE` invisibly if the configuration is valid. Stops with an
informative message otherwise.

## Details

Checks performed:

- `port_construction_method` is a single, non-`NA` character among the
  methods a sub-portfolio may use ('ew', 'sw', 'cw', 'cs', 'rp', 'hrp',
  'mvo'). Layered methods are excluded on purpose: nesting one layered
  method inside another is not supported.

- the parameter object matching that method, when supplied, has the
  expected S4 class. Parameter objects that do not match the method are
  left untouched (they are inert, not invalid).
