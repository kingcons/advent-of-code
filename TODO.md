* Automate away the tedious editing of individual year defsections in main.lisp
  similar to how I automated away the same editing in overview.lisp.
  This would make it so a new day only touches: advent.asd, dayNN.lisp, dayNN.dat
  The primary problem I can think of is that since these are directly referenced
  as separate sections in the top-level @advent there may not be a good way to
  include them short of grouping all the years under another section called "Years" or something.
  On the bright side, that may prevent us nesting down to the level of Part 1, Part 2.
  Probably worth it to me.
