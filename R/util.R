# replaces each special character by a space

replaceSpecialChars <- function(d, language) {

  if(language      == "english")    gsub("[^a-z]", " ", d)
  else if(language == "german")     gsub("[^a-z\u00E4\u00F6\u00FC\u00DF]", " ", d)
  else if(language == "french")     gsub("[^a-z\u00E9\u00E1\u00E0\u00E8\u00F9\u00E2\u00EA\u00EE\u00F4\u00FB\u00E7\u00EB\u00EF\u00FC]", " ", d)
  else if(language == "portuguese") gsub("[^a-z\u00E0\u00E2\u00E3\u00E1\u00E7\u00E9\u00EA\u00ED\u00F3\u00F4\u00F5\u00FA]", " ", d)
  else if(language == "spanish")    gsub("[^a-z\u00E1\u00E9\u00ED\u00F1\u00F3\u00FA\u00FC]", " ", d)
  else if(language == "swedish")    gsub("[^a-z\u00F6\u00E5]", " ", d)
  else if(language == "russian")    gsub("[^a-\u042F\u0451]", " ", d)
  else if(language == "italian")    gsub("[^a-z\u00E0\u00E8\u00E9\u00F2\u00F9\u00EC]", " ", d)
}
