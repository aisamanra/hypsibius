**Hypsibius** is going to be a music tracker with pluggable notions of tuning and temperament, which will permit historical tunings such as [just intonation](https://en.wikipedia.org/wiki/Just_intonation) or [quarter-comma meantone](https://en.wikipedia.org/wiki/Quarter-comma_meantone), microtonal tunings such as [nineteen equal temperament](https://en.wikipedia.org/wiki/19_equal_temperament) or [fifty-three tone equal temperament](https://en.wikipedia.org/wiki/53_equal_temperament), and even non-octave-repeating tunings like the [Bohlen-Pierce scale](https://en.wikipedia.org/wiki/Bohlen%E2%80%93Pierce_scale) or Wendy Carlos' [Alpha](https://en.wikipedia.org/wiki/Alpha_scale) and [Beta](https://en.wikipedia.org/wiki/Beta_scale) scales.

This is in a very early state, but the plan is to build the GUI with [`brick`](http://hackage.haskell.org/package/brick) and do the sounds with [`csound`](http://csound.github.io/). Some early scale files (which will define the scales used by Hypsibius) can be found in the `data/scales` directory.
