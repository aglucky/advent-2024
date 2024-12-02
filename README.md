# Advent of Code 2024 Solutions

This is my set of solutions for the 2024 Advent of Code.
I chose Haskell this year to avoid writing for loops.

To run, ensure you have [GHCup](https://www.haskell.org/ghcup/) install with `runhaskell` in your `$PATH`.

## Example

```console
curl https://adventofcode.com/2024/day/1/input -H 'Cookie: session=<session_id>' > input.txt
runhaskell day1/solution.hs
```

## Notes

### To get session_id, Use the following steps

1. Log-in to advent of code and click on "get your puzzle input" on any day.
2. Open developer tools on the page with your input and refresh the browser.
3. Navigate to the Network section and looks for a request named "input".
4. Open the "Headers" tab for the request and look at its corresponding cookies
5. The session_id will be shown as part of the cookie in format "session=<session_id>"
