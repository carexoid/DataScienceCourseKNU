# directory - шлях до папки з фалами спостереження
# pollutant - забрудник(сульфат або нітрат)
# id - перелік моніторів, з якими ми працюємо
pmean <- function(directory, pollutant, id=1:332){
  # для кожного id створюємо строку виду "001.csv"
  # filenames - список імен файлів 
  filenames <- sprintf("%03d.csv", id)
  # перетворюємо імена в шляхи до цих файлів
  # filenames тепер список шляхів до файлу 
  filenames <- file.path(directory, filenames)
  # для кожного файлу виконуємо функцію read.csv
  list_of_dataframes <- lapply(filenames, read.csv)
  # склеюємо всі dataframes
  big_dataframe <- do.call("rbind", list_of_dataframes)
  # знаходимо середнє значення, видаляємо всі NA
  mean(big_dataframe[[pollutant]], na.rm = TRUE)
}

complete <- function(directory, id){
  # для кожного id створюємо строку виду "001.csv"
  # filenames - список імен файлів 
  filenames <- sprintf("%03d.csv", id)
  # перетворюємо імена в шляхи до цих файлів
  # filenames тепер список шляхів до файлу 
  filenames <- file.path(directory, filenames)
  # для кожного файлу виконуємо функцію read.csv
  list_of_dataframes <- lapply(filenames, read.csv)
  # для кожного dataframe находимо список повних спостережень
  cc <- lapply(list_of_dataframes, complete.cases)
  # знаходимо суму повних спостережень
  cc_sum <- lapply(cc, sum)
  # повертаємо dataframe
  data.frame(id = id, nobs = I(cc_sum))
}

corr <- function(directory, threshold=0){
  # Створюємо список шляхів до всіх csv файлів в папці
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  # для кожного файлу виконуємо функцію read.csv
  list_of_dataframes <- lapply(filenames, read.csv)
  # Створення пустого вектору
  res <- c()
  # для кожного dataframe в list_of_dataframes
  for (df in ldf){
    # якщо к-сть повних стостережень більше threshold
    if (sum(complete.cases(df)) > threshold){
      # Записуємо в вектор-результат кореляції між сульфатами та нітратами
      res<-c(res, cor(df[['sulfate']], df[['nitrate']], use='pairwise.complete.obs'))
    }
  }
  return(res)
}