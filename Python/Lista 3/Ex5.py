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
  temp = ""

  for i in range(len(text)):
    while i != len(text) and text[i].isnumeric():
      temp += text[i]
      i += 1
    if temp: 
      decompressed_text.append((int(temp) - 1) * text[i])
    else:
      decompressed_text.append(text[i])
    temp = ""

  return ''.join(decompressed_text)


compressed_text = "2C2ol s3u2m4er w4ith gr2andm20a."
decompressed_text = "CCool suuummeeeer wiiiith graandmaaaaaaaaaaaaaaaaaaaa."

print(compress(decompress(compressed_text)))
print(decompress(compress(decompressed_text)))