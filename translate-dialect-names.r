# Estonian to English
myDF <- mutate(myDF, Dialect_en = ifelse(Dialect_et == "Ida", "Eastern",
                                         ifelse(Dialect_et == "Kesk", "Mid",
                                                ifelse(Dialect_et == "Lääne", "Western",
                                                       ifelse(Dialect_et == "Kirde", "Northeastern",
                                                              ifelse(Dialect_et == "Mulgi", "Mulgi",
                                                                     ifelse(Dialect_et == "Ranna", "Coastal",
                                                                            ifelse(Dialect_et == "Saarte", "Insular",
                                                                                   ifelse(Dialect_et == "Setu", "Setu",
                                                                                          ifelse(Dialect_et == "Tartu", "Tartu",
                                                                                                 ifelse(Dialect_et == "Võru", "Võru", "NA")))))))))))
# English to Estonian
myDF <- mutate(myDF, Dialect_et = ifelse(Dialect_en == "Eastern", "Ida",
                                         ifelse(Dialect_en == "Mid", "Kesk",
                                                ifelse(Dialect_en == "Western", "Lääne",
                                                       ifelse(Dialect_en == "Northeastern", "Kirde",
                                                              ifelse(Dialect_en == "Mulgi", "Mulgi",
                                                                     ifelse(Dialect_en == "Coastal", "Ranna",
                                                                            ifelse(Dialect_en == "Insular", "Saarte",
                                                                                   ifelse(Dialect_en == "Setu", "Setu",
                                                                                          ifelse(Dialect_en == "Tartu", "Tartu",
                                                                                                 ifelse(Dialect_en == "Võru", "Võru", "NA")))))))))))