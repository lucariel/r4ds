##STRINGS##
library(tidyverse)
library(stringr)

x <- c("amigo", "volar", "violin")
x
writeLines(x)
str_length(x)
str_c(x[1], x[2], sep = ",")


y <- c("Apple", "Banana", "Pear")
str_sub(y, 1,3)
str_sub(y, -3,-1)
str_c(str_sub(y, 1,3),str_sub(y, -3,-1) )
##14.2.5 Exercises

##1.In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?
?paste()
?paste0()
c0<-paste0(x, y)
s0<-paste(x, y)
#The difference between paste() and paste0() is that the argument sep by default is ” ” (paste) and “” (paste0).


##2.In your own words, describe the difference between the sep and collapse arguments to str_c().
?str_c()
length(str_c(x, y, collapse = ","))
length(str_c(x, y, sep = ","))

#Collapse returns a vector of value 1, sep keeps size of vector of the combined strings


##3.Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?
s<- "Aminoacido"
str_sub(s, str_length(s)/2,str_length(s)/2+1)


##4.What does str_wrap() do? When might you want to use it?

?str_wrap()
#implements the Knuth-Plass paragraph wrapping algorithm. Which format paragraphs so they are well writen.
#It might be usefull to publish, or to read better a text when it is imported.

##5.What does str_trim() do? What’s the opposite of str_trim()?

?str_trim()
#str_trim() removes whitespace from start and end of string;

g<- "                 this a house       "
h<-str_trim(g)
writeLines(h)
writeLines(g)

#and the oposite would be 
str_trim(str_pad(h, 30, side=c("both"), pad = " "))
#to add whitespace (or other). The difference is that they are not
#perfect oposites since you could pad it with other character than whitespaces and it would be trimmed
str_trim(str_pad(h, 30, side=c("both"), pad = "."))


##6.Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.
vectorToString<-function(A){
  if(length(A)>= 3){
  x<-str_c(str_c(A[1], A[2], sep = ", "), A[3], sep = " and ")
  return(x)
  }
  if(length(A)== 2){
    return(str_c(A[1], A[2], sep = " and "))
  }
  return(A)
}
vectorToString(c("a"))


##14.3 Matching patterns with regular expressions
##14.3.1 Basic matches
x <- c("apple", "banana", "pear")
str_view(x, ".a.")


##1.Explain why each of these strings don’t match a \: "\", "\\", "\\\".
#"\" is the escape character so the string "\" cannot be made, you have to scape the scape
c("\\\\")
str(c("\\"))
c<-c("\\")
str_view(c, "\\")
#"\\" throws error::Unrecognized backslash escape sequence in pattern. (U_REGEX_BAD_ESCAPE_SEQUENCE)
#"\\\" leaves the str open, since R reads it as escaping the last "
str_view(c,"\\\\")

#2.How would you match the sequence "'\?
c<-c("\"\'\\?")
writeLines(c)
str_view(c, "\"\'\\\\")

#3.What patterns will the regular expression \..\..\.. match? How would you represent it as a string?
d<-c(".a.a.a")
str_view(d,'\\..\\..\\..')


# Anchors> ^ $ or \b
#1.How would you match the literal string "$^$"?
c<-c("$^$")
str_view(c, "\\$\\^\\$")
str_view(c, "[$$]")
#2. Given the corpus of common words in stringr::words, create regular expressions that find all words that:
#   ## a.Start with “y”.
# b.End with “x”
# Are exactly three letters long. (Don’t cheat by using str_length()!)
# Have seven letters or more.
# 
# Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.

stringr::words
?str_view()
#a.
str_view(stringr::words, "^t", match = T)

#b.
str_view(stringr::words, "x$", match = T )

#c.
str_view(stringr::words, "\\b.{3}\\b", match = T )
#d.
str_view(stringr::words, "\\b.{7,}\\b", match = T )

#14.3.3 Character classes and alternatives
#1.Create regular expressions to find all words that:

#a) Start with a vowel.
c<-c("asd")
str_view(stringr::words, "[aeiou]") 
#b) That only contain consonants. (Hint: thinking about matching “not”-vowels.)
str_view(stringr::words, "^[^aeiou]", match = T ) 

#c) End with ed, but not with eed.
str_view(stringr::words, "[^e]ed$", match = T ) 

#d) End with ing or ise.
str_view(stringr::words, "(ise)$", match = T ) 
#2.Empirically verify the rule “i before e except after c”.
str_view(stringr::words, "ie|[^c]ie", match =  T)
#3.Is “q” always followed by a “u”?
d<-c("qiso")
str_view(stringr::words, "q[^u]", match =  T)
##Yes
#4.Write a regular expression that matches a word if it’s probably written in British English, not American English.
#5.Create a regular expression that will match telephone numbers as commonly written in your country.
num<-c("1130573956")
str_view(num, "11\\d{8}")


#14.3.4.1 Exercises

#1)Describe the equivalents of ?, +, * in {m,n} form.
##? => {0,1}
##+ => {1,}
##+ => {0,}
#2)Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)
#^.*$ any one letter word (regex)
#"\\{.+\\}" finds any word between brackets (str that define regex)
#\d{4}-\d{2}-\d{2} finds a number of theformat nnnn-nn-nn (regex)
#"\\\\{4}" ##finds four backlash

#3.Create regular expressions to find all words that:
#a.Start with three consonants.
str_view(stringr::words, "(^[^aeiou]{3})", match =  T)
#b.Have three or more vowels in a row.
str_view(stringr::words, "[aeiou]{3,}", match = T)
#c.Have two or more vowel-consonant pairs in a row.
str_view(stringr::words, "([aeiou][^aeiou]){2}", match = T)



#14.3.5.1 Exercises

#1.Describe, in words, what these expressions will match:


#a>   (.)\1\1
##Three equal letters 
c<-("aaaal")
str_view(c, "(.)\\1\\1", match = T)

#b>"(.)(.)\\2\\1"
c<-("asasasas")

str_view(c, "(.)(.)\\2\\1", match = T)
##Four equal letter

#c> (..)\1
c<-("asasasas")

str_view(c, "(..)\\1", match = T)

##Two consecutive repeating groups of two letters

#d> "(.).\\1.\\1"

c<-("anaga")

str_view(c, "(.).\\1.\\1", match = T)
## A group of a xYxZx

#e> (.)(.)(.).*\\3\\2\\1"
c<-"asdllllllllllllllllldsa"
str_view(c, "(.)(.)(.).*\\3\\2\\1", match = T)
##A mirror pattern with anything in the middle



#14.4.1.1 Exercises

#For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.

#1)Find all words that start or end with x.
str_view(words, "^x|x$", match =  T)
str_detect(words, "^x|x$")
str_detect(c(words[str_detect(words, "^x|x$")], "xilophone"),"^x|x$")

#2)Find all words that start with a vowel and end with a consonant.
str_view(words, "^[aeiou].*[^aeiou]$", match = T)
w1<-words[str_detect(words, "^[aeiou].*[^aeiou]$")]

s_vowel<-str_detect(words, "^[aeiou]")

e_no_vowel<-str_detect(words, "[^aeiou]$")
w2<-words[s_vowel&e_no_vowel]
identical(w1,w2)
#3)Are there any words that contain at least one of each different vowel?
a<-str_detect(words, "a")
e<-str_detect(words, "e")
i<-str_detect(words, "i")
o<-str_detect(words, "o")
u<-str_detect(words, "u")
words[a&e&i&o&u]#no, there is not

#2.What word has the highest number of vowels? 
df <- tibble(
  word = words, 
  i = seq_along(word)
)

df2<-df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )%>%
  arrange(desc(vowels))%>%
  filter(
    vowels == 5
  )
df2$word #Highest number of vowels
#What word has the highest proportion of vowels? (Hint: what is the denominator?)
df3<-df%>%
  mutate(
    len = nchar(word),
    vowels = str_count(word, "[aeiou]"),
    vowel_prop = vowels/len
  )%>%
  arrange(desc(vowel_prop))%>%
  head()
df3$word #Highest prop of vowels


length(sentences)
head(sentences)
colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match<-str_c(colours, collapse = "|")
?str_c() ##Joins multiple strings

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

#14.4.2.1 Exercises

#1.In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.

colours <- c(" red", "orange", "yellow", "green", "blue", "purple")
colour_match<-str_c(colours, collapse = "|")

##Works. 

#2.From the Harvard sentences data, extract:

#a.The first word from each sentence.

str_extract(sentences, "^[^\\s]+")%>% head()

#b.All words ending in ing.

m<-str_extract(sentences, "[^\\s]+ing")
m[!is.na(m)]

pattern <- "[^\\s]+ing"
sentences_with_ing <- str_detect(sentences, pattern)
unique(unlist(str_extract_all(sentences[sentences_with_ing], pattern))) %>%
  head()

#c.All plurals.
m<-str_extract(sentences, "[^\\s|\\']+s")
m[!is.na(m)]%>%head()



