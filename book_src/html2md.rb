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
  `pandoc #{s}.html -o #{(i+1).to_s.rjust(2, '0')}-#{s}.md`
end
