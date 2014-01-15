# encoding: UTF-8

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

# some character that is unlikely to be in source files
UNIQ_CHAR = '±'

sections.each_with_index do |s, i|
  # join all lines into one using UNIQ_CHAR as separator
  `sed ':a;N;$!ba;s/\\n/#{UNIQ_CHAR}/g' < #{s}.html > #{s}_.html`
  `mv #{s}_.html #{s}.html`

  # that was a workaround to simplify replacement of tags opened on one line and closed on another :)
  `perl -i.original -pe 's/<span class="fixed">(.*?)<\\/span>/<code class="fixed">\\1<\\/code>/g' #{s}.html`
  `perl -i.original -pe 's/<span class="label([^"]*)">(.*?)<\\/span>/<code class="label\\1">\\2<\\/code>/g' #{s}.html`

  # split lines into original form
  `sed 's/#{UNIQ_CHAR}/\\n/g' < #{s}.html > #{s}_.html`
  `mv #{s}_.html #{s}.html`
end
