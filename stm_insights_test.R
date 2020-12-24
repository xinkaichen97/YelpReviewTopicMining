library(stm)
processed <- textProcessor(data$REASON, metadata = data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

PreFit <- stm(documents = out$documents, vocab = out$vocab,
              K = 5, prevalence =~ Joe + liberal_attitude + 
                Joe*liberal_attitude,
              max.em.its = 75, data = out$meta,
              init.type = "Spectral")


prep <- estimateEffect(1:5 ~ Joe + liberal_attitude + Joe*liberal_attitude, 
                       PreFit,
                       meta = out$meta, uncertainty = "Global")

fix.encoding <- function(df, originalEncoding = "ISO-8859-1 ") {
  numCols <- ncol(df)
  df <- data.frame(df)
  for (col in 1:numCols)
  {
    if(class(df[, col]) == "character"){
      Encoding(df[, col]) <- originalEncoding
    }
    
    if(class(df[, col]) == "factor"){
      Encoding(levels(df[, col])) <- originalEncoding
    }
  }
  return(as_tibble(df))
}

out$meta <- fix.encoding(out$meta)
out$meta$document <-  as.character(out$meta$document)

rm(processed, storage, fix.encoding)
save.image("bcorp_clean.RData")

save.image('stm_gadarian.RData')
library(stminsights)
run_stminsights()
