sections = %w{
  introduction
  starting-out
  types-and-typeclasses
  syntax-in-functions
  recursion
  higher-order-functions
  modules
  making-our-own-types-and-typeclasses
  input-and-output
  functionally-solving-problems
  functors-applicative-functors-and-monoids
  a-fistful-of-monads
  for-a-few-monads-more
  zippers
}

sections.each_with_index do |s, i|
  `perl -i.original -pe 's/<span class="fixed">(.*?)<\\/span>/<code class="fixed">\\1<\\/code>/g' #{s}.html`
  `perl -i.original -pe 's/<span class="label([^"]*)">(.*?)<\\/span>/<code class="label\\1">\\2<\\/code>/g' #{s}.html`
end
