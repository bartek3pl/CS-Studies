import random

def text_shrink(text, length):
  return list(filter(lambda x: len(x) <= length, text.split(' ')))

def del_random_words(text, amount):
  while len(text) > amount:
    text.remove(random.choice(text))
  return text

def simplify_sentence(text, length, amount):
  return del_random_words(text_shrink(text, length), amount)


text = "Lithuania, my country! You are as good health: \
How much one should prize you, he only can tell \
Who has lost you. Your beauty and splendour I view \
And describe here today, for I long after you."

print(simplify_sentence(text, 3, 5))

