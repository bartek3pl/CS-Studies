def compress(text):
  compressed_text = []
  i = 0

  while i < len(text):
    counter = 0
    j = i
    while j != len(text) - 1 and text[j] == text[j+1]:
      counter += 1
      j += 1
    if counter < 1:
      compressed_text.append(text[i])
    else:
      compressed_text.append(str(counter + 1) + text[i])
      i += counter
    i += 1
    
  return ''.join(compressed_text)  

def decompress(text):
  decompressed_text = []

  for i in range(len(text)):
    if text[i].isnumeric():
      decompressed_text.append((int(text[i]) - 1) * text[i + 1])
    else:
      decompressed_text.append(text[i])

  return ''.join(decompressed_text)


compressed_text = "2C2ol s3u2m4er w4ith gr2andm9a."
decompressed_text = "CCool suuummeeeer wiiiith graandmaaaaaaaaa."

print(compress(decompress(compressed_text)))
print(decompress(compress(decompressed_text)))