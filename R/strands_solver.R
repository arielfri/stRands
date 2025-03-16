
get.word <- function(x, puzzle) {
  x <- x[is.na(x)==FALSE]
  ints <- matrix(as.integer(unlist(strsplit(x, split = ","))), ncol = 2, byrow = TRUE)
  puzzle[ints]
}

get.words <- function(x, puzzle) {
  words <- apply(x, 1, get.word, puzzle)
  if (is.matrix(words)) return(apply(words, 2, FUN = paste0, collapse = ""))
  if (is.list(words)) return(unlist(lapply(words, FUN = paste0, collapse = "")))
}

surrounding.pos <- function(x) {
  spl <- strsplit(x, split = ",")[[1]]
  row <- as.integer(spl[1])
  col <- as.integer(spl[2])
  c(paste0(row+1L, ",", col+1L),
    paste0(row   , ",", col+1L),
    paste0(row-1L, ",", col+1L),
    paste0(row+1L, ",", col   ),
    paste0(row-1L, ",", col   ),
    paste0(row+1L, ",", col-1L),
    paste0(row   , ",", col-1L),
    paste0(row-1L, ",", col-1L))}

mat_remove_words <- function(rows, mat) {
  row_check <- mat[rows,]

  if (length(rows)==1) remove_cols <- names(row_check[row_check>=1])
  else remove_cols <- colnames(row_check)[colSums(row_check)>=1]

  mat <- mat[rowSums(mat[,remove_cols])==0,, drop = FALSE]
  mat <- mat[,!(colnames(mat) %in% remove_cols), drop = FALSE]

  mat <- mat[,names(sort(colSums(mat))), drop = FALSE]

  if (ncol(mat)==0) return(mat)
  if (nrow(mat)==0) return(mat)
  else return(mat[order(-mat[,1]),, drop = FALSE])
}

crosses <- function(x) {
  ints <- matrix(as.integer(unlist(strsplit(x[is.na(x)==FALSE], split = ","))), ncol = 2, byrow = TRUE, dimnames = list(NULL, c("row", "col")))
  data.frame(ints, rbind(matrix(c(NA, NA), ncol = 2, dimnames = list(NULL, c("row_prev", "col_prev"))), ints[1:(nrow(ints)-1),]), rbind(matrix(c(NA, NA), ncol = 2, dimnames = list(NULL, c("row_dif", "col_dif"))), diff(ints)))
}

check_cross_word <- function(word_df) {
  if(nrow(word_df)<=3) return(FALSE)

  word_df <- word_df[2:nrow(word_df),]
  word_df$seq <- paste0(word_df$row, ",", word_df$col, ";", word_df$row_prev, ",", word_df$col_prev)

  res <- c()

  for (i in 1:nrow(word_df)) {
    if (word_df$row_dif[i]==1 & word_df$col_dif[i]==1) {
      res[i] <- any(c(paste0(word_df$row_prev[i], ",", word_df$col_prev[i]+1, ";", word_df$row_prev[i]+1, ",", word_df$col_prev[i]), paste0(word_df$row_prev[i]+1, ",", word_df$col_prev[i], ";", word_df$row_prev[i], ",", word_df$col_prev[i]+1)) %in% word_df$seq)
    } else if (word_df$row_dif[i]==-1 & word_df$col_dif[i]==-1) {
      res[i] <- any(c(paste0(word_df$row_prev[i]-1, ",", word_df$col_prev[i], ";", word_df$row_prev[i], ",", word_df$col_prev[i]-1), paste0(word_df$row_prev[i], ",", word_df$col_prev[i]-1, ";", word_df$row_prev[i]-1, ",", word_df$col_prev[i])) %in% word_df$seq)
    } else if (word_df$row_dif[i]==1 & word_df$col_dif[i]==-1) {
      res[i] <- any(c(paste0(word_df$row_prev[i]+1, ",", word_df$col_prev[i], ";", word_df$row_prev[i], ",", word_df$col_prev[i]-1), paste0(word_df$row_prev[i], ",", word_df$col_prev[i]-1, ";", word_df$row_prev[i]+1, ",", word_df$col_prev[i])) %in% word_df$seq)
    } else if (word_df$row_dif[i]==-1 & word_df$col_dif[i]==1) {
      res[i] <- any(c(paste0(word_df$row_prev[i], ",", word_df$col_prev[i]+1, ";", word_df$row_prev[i]-1, ",", word_df$col_prev[i]), paste0(word_df$row_prev[i]-1, ",", word_df$col_prev[i], ";", word_df$row_prev[i], ",", word_df$col_prev[i]+1)) %in% word_df$seq)
    } else {
      res[i] <- FALSE
    }
  }
  any(res)
}

#' Strands Solver
#'
#' This function solves the New York Times Strands word game. It works in 2 stages: In the first stage it finds all the words on the board, and in the second stage it tries to find combinations of words that cover the board and form solutions.
#'
#' @param puzzle Matrix of puzzle letters, see example below for formatting.
#' @param dictionary_freq A data frame, where the first column is the dictionary of words to use to search for puzzle solutions, and the second column is the frequency of the corresponding word. The default dictionary is `data(unigram_freq)`.
#' @param max_words_checked Maximum number of words to use in stage 2 when searching for puzzle solutions.
#' @param word_minimum_characters Minimum character length for words to be included in the solution.
#' @param include_all_words_with_at_least_nchars When searching for solutions in stage 2, use all words that exceed this character count when searching for puzzle solutions.
#' @param remove_crosses Do not include words that have letters which cross over each other.
#' @param return_word_list_only Only return the words found, not final solutions.
#' @param check_solutions Check to make sure that all solutions use every letter exactly once.
#' @param verbose Include additional output showing the progress.
#'
#' @return A `data.table` with solutions. The column:
#' \itemize{
#'  \item{`word`: }{The word that was found.}
#'  \item{`solution_number`: }{A unique ID for each solution.}
#'  \item{`n_words`: }{The number of words in each solution.}
#'  \item{`freq`: }{The frequency of the word taken from the dictionary.}
#'  \item{`min_freq`: }{The frequency of the least frequent word in the solution.}
#'  \item{`mean_freq`: }{The mean frequency of the words in the solution.}
#'  \item{`letter_1`, `letter_2`, etc.: }{The location of the letters in each word on the board (row number, column number).}
#' }
#'
#' @examples
#' #From New York Times, January 18, 2025
#'
#' #Hint: O ___! My ___!:
#' #Solution: crunch obvious planet kangaroo hook captain underpants
#'
#' \preformatted{puzzle <- matrix(data = c(
#'  "T", "E", "N", "H", "C", "N",
#'  "O", "O", "K", "A", "U", "C",
#'  "R", "A", "H", "O", "L", "R",
#'  "A", "N", "P", "T", "O", "P",
#'  "G", "A", "N", "A", "K", "N",
#'  "C", "E", "D", "U", "I", "O",
#'  "R", "P", "A", "V", "B", "S",
#'  "S", "T", "N", "I", "O", "U"
#'  ), nrow = 8, byrow = TRUE)}
#'
#' strands_solver(puzzle = puzzle, dictionary_freq = unigram_freq)
#' @export
strands_solver <- function(puzzle, dictionary_freq, max_words_checked = 1000, word_minimum_characters = 4, include_all_words_with_at_least_nchars = 6, remove_crosses = TRUE, return_word_list_only = FALSE, check_solutions = TRUE, verbose = TRUE) {

  names(dictionary_freq) <- c("word", "freq")
  dictionary_freq$word <- tolower(dictionary_freq$word)
  dictionary_freq <- dictionary_freq[is.na(dictionary_freq$word)==FALSE & is.na(dictionary_freq$freq)==FALSE,]
  dictionary_freq <- dictionary_freq[duplicated(dictionary_freq$word)==FALSE,]

  dictionary <- dictionary_freq$word

  puzzle <- tolower(puzzle)

  index <- expand.grid(1:dim(puzzle)[1], 1:dim(puzzle)[2])
  names(index) <- c("row", "col")
  index$row_col <- paste0(index$row, ",", index$col)

  final_list <- data.frame()

  t <- Sys.time()
  for (letter in 1:length(index$row_col)) {

    pos <- index[letter,3]

    check_list <- data.frame(letter_1 = pos)

    while(nrow(check_list)>0) {

      check_list_update <- data.frame()

      for (i in 1:nrow(check_list)) {

        current_row <- as.data.frame(check_list[i,])
        names(current_row) <- NULL
        row.names(current_row) <- NULL

        dim_to_check <- current_row[is.na(current_row)==FALSE]
        dim_to_check_last_letter <- dim_to_check[length(dim_to_check)]

        dims_to_check <- surrounding.pos(dim_to_check_last_letter)

        dims_to_check <- dims_to_check[dims_to_check %in% index$row_col]

        dims_to_check <- dims_to_check[!(dims_to_check %in% dim_to_check)]

        if (length(dims_to_check)==0) next

        words <- get.words(data.frame(current_row, dims_to_check, row.names = 1:length(dims_to_check)), puzzle = puzzle)

        len <- max(nchar(words))
        while (!exists(paste0("dictionary_", len)) & len>=2L) {
          assign(paste0("dictionary_", len), unique(dictionary[nchar(dictionary)==len]))
          len <- len-1L
        }

        len <- max(nchar(words))
        while (!exists(paste0("dictionary_start_", len)) & len>=2L) {
          assign(paste0("dictionary_start_", len), unique(substr(dictionary[nchar(dictionary)>len], 1L, len)))
          len <- len-1L
        }

        add_to_final_list <- pinp_char(words, get(paste0("dictionary_", max(nchar(words)))))
        add_to_check_list <- pinp_char(words, get(paste0("dictionary_start_", max(nchar(words)))))

        if (sum(add_to_final_list)>0) {
          final_list_to_add <- data.frame(cbind(current_row, dims_to_check[add_to_final_list]))
          names(final_list_to_add) <- paste0("letter_", 1:ncol(final_list_to_add))
          final_list <- as.data.frame(data.table::rbindlist(l = list(final_list, final_list_to_add), use.names = TRUE, fill = TRUE))
        }

        if (sum(add_to_check_list)>0) {
          check_list_to_add <- data.frame(cbind(current_row, dims_to_check[add_to_check_list]))
          names(check_list_to_add) <- paste0("letter_", 1:ncol(check_list_to_add))
          check_list_update <- as.data.frame(data.table::rbindlist(l = list(check_list_update, check_list_to_add), use.names = TRUE, fill = TRUE))
        }
      }
      check_list <- check_list_update
    }
  }
  if (verbose) {diff_time <- Sys.time()-t; cat("Time to find all words: ", round(diff_time,2), " ", attributes(diff_time)$units, "\n", sep = "")}

  if(remove_crosses==TRUE & nrow(final_list)>0) {
    crosses_list <- apply(final_list, 1, crosses)
    final_list$cross <- sapply(crosses_list, check_cross_word)
    rm(crosses_list)
    final_list <- final_list[final_list$cross==FALSE,]
    final_list$cross <- NULL
    if(nrow(final_list)>0) row.names(final_list) <- 1:nrow(final_list)
  }

  if(nrow(final_list)==0) {
    cat("No words found.")
    return()
  }

  if (return_word_list_only) {
    final_list$word <- get.words(final_list, puzzle = puzzle)
    final_list <- as.data.table(final_list)
    setcolorder(final_list, c("word", names(final_list)[names(final_list)!="word"]))
    return(final_list)
  }

  mat_words <- matrix(data = NA, ncol = length(index$row_col), dimnames = list(NULL, index$row_col))
  mat_words <- mat_words[-1,, drop = FALSE]

  for (i in 1:nrow(final_list)) {
    word <- unlist(final_list[i,])
    word <- word[is.na(word)==FALSE]
    mat_words <- rbind(mat_words, as.integer(index$row_col %in% word))
  }
  rm(word)
  row.names(mat_words) <- 1:nrow(mat_words)

  final_list$word <- get.words(final_list, puzzle = puzzle)

  candidates <- final_list
  candidates$order <- 1:nrow(candidates)
  candidates <- merge(x = candidates, y = dictionary_freq[,c("word", "freq")], by = "word", all.x = TRUE)
  candidates <- candidates[order(candidates$order),]
  row.names(candidates) <- NULL

  candidates <- candidates[is.na(candidates$freq)==FALSE,]
  candidates <- candidates[order(-candidates$freq),]
  candidates$nchar <- nchar(candidates$word)

  candidates_intermediate_length <- candidates[candidates$nchar<include_all_words_with_at_least_nchars & candidates$nchar>=word_minimum_characters,]

  candidates <- candidates[candidates$nchar>=include_all_words_with_at_least_nchars,]
  candidates <- rbind(candidates, candidates_intermediate_length[seq_len(max(0, min(nrow(candidates_intermediate_length), max_words_checked-nrow(candidates)))),])

  mat_words <- mat_words[row.names(mat_words) %in% row.names(candidates),, drop = FALSE]

  if(nrow(mat_words)==1) {
    if(all(mat_words==1)) {
      cat("Solution is one word.")
      return(as.data.table(candidates[,c("word", names(candidates)[substr(names(candidates), 1, 7)=="letter_"])]))
    } else {
      final_list <- as.data.table(final_list)
      setcolorder(final_list, c("word", names(final_list)[names(final_list)!="word"]))
      cat("No solutions found. Returning the list of words found.")
      return(final_list)
    }
  }

  mat_words <- mat_words[,names(sort(colSums(mat_words)))]
  mat_words <- mat_words[order(-mat_words[,1]),]

  check_list <- list()
  words_rowname <- c()
  solutions <- list()

  t <- Sys.time()
  for (i in row.names(mat_words)[mat_words[,1]==1]) {

    words_rowname <- i

    check_list <- list()
    check_list[[1]] <- i

    iter <- 0
    iterate <- TRUE

    if (verbose) cat("\n", which(row.names(mat_words)[mat_words[,1]==1]==i), "/", length(row.names(mat_words)[mat_words[,1]==1]), " ", i, "\n", sep = "")

    while(iterate) {

      iter <- iter+1

      elim <- mat_remove_words(rows = words_rowname, mat = mat_words)

      if (ncol(elim)==0 | nrow(elim)==0) {
        if (ncol(elim)==0) solutions[[length(solutions)+1]] <- words_rowname
        lens <- unlist(lapply(check_list, length))
        check_list_g2 <- which(lens>1)
        if(length(check_list_g2)>0) {
          check_list <- check_list[1:max(check_list_g2)]
          check_list[[max(check_list_g2)]] <- check_list[[max(check_list_g2)]][-1]
          words_rowname <- unlist(lapply(check_list, data.table::first))
        } else {
          iterate <- FALSE
        }
      } else {
        if(sum(elim[,1])!=0) {
          check_list[[length(check_list)+1]] <- row.names(elim)[elim[,1]==1]
          words_rowname[length(words_rowname)+1] <- data.table::first(check_list[[length(check_list)]])
        } else if (sum(elim[,1])==0) {
          if(length(check_list[[length(check_list)]])>1) {
            check_list[[length(check_list)]] <-check_list[[length(check_list)]][-1]
            words_rowname[length(words_rowname)] <- data.table::first(check_list[[length(check_list)]])
          } else if (length(check_list[[length(check_list)]])==1) {
            lens <- unlist(lapply(check_list, length))
            check_list_g2 <- which(lens>1)
            if(length(check_list_g2)>0) {
              check_list <- check_list[1:max(check_list_g2)]
              check_list[[max(check_list_g2)]] <- check_list[[max(check_list_g2)]][-1]
              words_rowname <- unlist(lapply(check_list, data.table::first))
            } else {
              iterate <- FALSE
            }
          }
        }
      }
      if (verbose & iter %% 10000==0) cat(iter, " - ", length(check_list[[2]]), " - ", length(solutions), "; ", sep = "")
    }
    if (verbose) cat("\n")
  }
  if (verbose) {diff_time <- Sys.time()-t; cat("Time to find solutions: ", round(diff_time,2), " ", attributes(diff_time)$units, "\n", sep = "")}

  if (length(solutions)==0) {
    final_list <- as.data.table(final_list)
    setcolorder(final_list, c("word", names(final_list)[names(final_list)!="word"]))
    cat("No solutions found. Returning the list of words found.")
    return(final_list)
  }

  if (check_solutions) {
    sols_check <- c()
    for (i in 1:length(solutions)) {sols_check[i] <- all(colSums(mat_words[solutions[[i]],])==1)}
    cat("Solutions check passed: ", all(sols_check), "\n", sep = "")
  }

  all_solutions <- data.table::rbindlist(l = lapply(solutions, function (x) data.frame(final_list[as.numeric(x),], n_words = length(x))), idcol = "solution_number")
  all_solutions <- merge(x = all_solutions, y = dictionary_freq[,c("word", "freq")], by = "word", all.x = TRUE)

  all_solutions[,`:=`(min_freq = min(freq), mean_freq = mean(freq)), by = solution_number]
  data.table::setorder(all_solutions, n_words, -min_freq, -mean_freq, solution_number)
  all_solutions[,`:=`(solution_number = .GRP), by = solution_number]
  data.table::setcolorder(all_solutions, c("word", "solution_number", "n_words", "freq", "min_freq", "mean_freq", names(all_solutions)[substr(names(all_solutions), 1, 7)=="letter_"]))
  return(all_solutions[])
}
