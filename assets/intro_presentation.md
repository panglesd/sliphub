# SlipHub

**Sliphub** is a hub for slipshow presentations. It helps you write one, and hosts it.

{.block}
> To create a new presentation:
>
> {style="text-align:center"}
> <button>[[Click here!](/new)]{style="text-align:center; font-size:3em"} </button>
>
> Then, **store the link** to be able to get back to the presentation later!

If you want to have more information about the syntax to write a presentation, [press the right arrow]{.emphasize}, several times.

{pause}

{#list}
- The syntax is markdown, with some extensions. a{pause style="visibility:hidden"}
- What's inside brackets is metadata. `{#my-id .theorem title="Fermat" pause}` means that the associated element:
  - has id `my-id`,
  - has class `theorem`,
  - has key `title` with value `"Fermat"`,
  - has flag `pause`. a{pause style="visibility:hidden" down-at-unpause=list}
- Attributes are associated to:
  - a block if they are the first line of the block
  - a word if they touch it (as in `word{#id}`),
  - an inline span if they are enclosed like in `[group of inline]{#id}`
  - or, standalone if they are separated from the rest by blanks. a{pause style="visibility:hidden"}
- Many attributes have a special meaning:
  - `{pause}` is a pause in the presentation
  - `up-at-unpause` puts the associated element on top of the screen at unpause
  - see this [doc](https://slipshow.readthedocs.io/)
