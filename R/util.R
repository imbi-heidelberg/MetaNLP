# replaces each special character by a space

replaceSpecialChars <- function(d, language) {

  if(language      == "english")    gsub("[^a-z]", " ", d)
  else if(language == "german")     gsub("[^a-z\u00E4\u00F6\u00FC\u00DF]", " ", d)
  else if(language == "french")     gsub("[^a-z\u00E9\u00E1\u00E0\u00E8\u00F9
                                         \u00E2\u00EA\u00EE\u00F4\u00FB\u00E7
                                         \u00EB\u00EF\u00FC]", " ", d)
  else if(language == "spanish")    gsub("[^a-z\u00E1\u00E9\u00ED\u00F1\u00F3
                                         \u00FA\u00FC]", " ", d)
  else if(language == "russian")    gsub("[^a-z\u0430\u0431\u0432\u0433\u0434
                                         \u0435\u0436\u0437\u0438\u0439\u043A
                                         \u043B\u043C\u043D\u043E\u043F\u0440
                                         \u0441\u0442\u0443\u0444\u0445\u0446
                                         \u0447\u0448\u0449\u044A\u044B\u044C
                                         \u044D\u044E\u044F\u0451]", " ", d)
}
