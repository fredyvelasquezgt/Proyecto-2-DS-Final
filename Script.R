db <- read.csv('train.csv')
View(db)
str(db)

plot(x = db$discourse_id)
plot(x = db$essay_id)
plot(x = db$discourse_text)
plot(x = db$discourse_type)
plot(x = db$discourse_effectiveness)
