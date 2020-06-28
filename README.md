# haskell-template

Encode and decode [Something](https://example.org)

Copy and use `./new PROJNAME` to rename stuff.

[Relative link](app/Main.hs).

## Section

* Bullet
* list

## Usage

```haskell
import qualified Data.Lib as C

main :: IO ()
main = do
  let x = (0, AnalogIn 4.48)
  print $ C.decode $ C.encode x
```
