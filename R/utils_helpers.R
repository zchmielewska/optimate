
LoadData <- function(table) {
  # Connect to the database
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  return(data)
}

LoadCurrentUsernames <- function() {
  # Connect to the database
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = "optimate_schema", host = options()$mysql$host, 
                          port = options()$mysql$port, user = options()$mysql$user, 
                          password = options()$mysql$password)
  
  # Construct the fetching query
  query <- "SELECT DISTINCT username FROM optimate_schema.answers;"
  
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  
  current.usernames <- data$username
  
  return(current.usernames)
}

ReturnScoresMatrix <- function(answers) {
  n <- nrow(answers)
  scores.matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(c(answers$username), c(answers$username)))
  for(i in 1:n) {
    for(j in 1:n) {
      scores.matrix[i, j] <- sum(answers[i, 2:11] == answers[j, 2:11])
    }
  }
  return(scores.matrix)
}

ReturnOptimalPairs <- function(scores.matrix) {
  n <- nrow(scores.matrix)
  
  # Subject to optimization is lower triangular
  f.obj <- c(scores.matrix * lower.tri(scores.matrix, diag = FALSE))
  
  # Conditions must be met
  f.con <- NULL
  
  # Maximal number of pairs is n/2 (floored)
  f.con <- rep(1, n^2)
  
  # In each row, sum must be lower or equal to 1
  for(i in 1:n) {
    m1 <- matrix(c(rep(0, (i-1)*n), rep(1, i), rep(0, n-i), rep(0, (n-i)*n)), byrow = TRUE, nrow = n)
    f.con <- rbind(f.con, c(m1))
  }
  
  # In each column, sum must be lower or equal to 1
  for(i in 1:n) {
    m2 <- matrix(c(rep(0, (i-1)*n), rep(0, i-1), rep(1, n-(i-1)), rep(0, (n-i)*n)), byrow = FALSE, nrow = n)
    f.con <- rbind(f.con, c(m2))
  }
  
  # Participants shouldn't be matched with themselves
  f.con <- rbind(f.con, c(diag(1, nrow = n)))
  
  # Pairs should be mirrored
  for(i in 1:n) {
    m <- matrix(rep(0, n^2), nrow = n)
    m[i,] <- 1
    m[,i] <- 1
    f.con <- rbind(f.con, c(m))
  }
  
  f.dir <- c("==",       rep("<=", 2*n), "==", rep("<=", n))
  f.rhs <- c(floor(n/2), rep(1,    2*n), 0,    rep(1,    n))
  
  res <- lpSolve::lp("max", f.obj, f.con, f.dir, f.rhs)
  solution <- matrix(res$solution, nrow = n)
  optimal.pairs <- solution + t(solution)
  
  rownames(optimal.pairs) <- rownames(scores.matrix)
  colnames(optimal.pairs) <- colnames(scores.matrix)
  
  return(optimal.pairs)
}

ReturnMatches <- function(scores.matrix, optimal.pairs) {
  
  scores.matrix.long <- tibble::as_tibble(scores.matrix, rownames = NA) %>%
    tibble::rownames_to_column(var = "username1") %>%
    tidyr::pivot_longer(cols = -c("username1"), names_to = "username2", values_to = "score")
  
  optimal.pairs.long <- tibble::as_tibble(optimal.pairs, rownames = NA) %>%
    tibble::rownames_to_column(var = "username1") %>%
    tidyr::pivot_longer(cols = -c("username1"), names_to = "username2", values_to = "match_indicator") %>% 
    dplyr::mutate(match_indicator = as.integer(match_indicator))
  
  matches <- dplyr::left_join(optimal.pairs.long, scores.matrix.long, by = c("username1", "username2")) %>%
    dplyr::filter(username1 != username2) %>% 
    dplyr::mutate(when_added = format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  return(matches)  
}

ReturnUsersAnswers <- function(username, answers, survey.table) {
  users.answers <- NULL
  
  numerical.answers <- dplyr::filter(answers, username == !!username) %>% 
    dplyr::select(paste0("q", 1:10)) %>% 
    tidyr::pivot_longer(everything(), names_to = "question", values_to = "answer") %>% 
    dplyr::select(answer) %>% 
    dplyr::pull()
  
  for(i in 1:10) {
    users.answers <- c(users.answers, (dplyr::pull(survey.table[i, paste0("answer", numerical.answers[i])])))
  }
  
  return(users.answers)
}

PrepareAndSaveToDB <- function(users.answers) {
  # Open connection
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = options()$mysql$dbname, host = options()$mysql$host, 
                          port = options()$mysql$port, user = options()$mysql$user, 
                          password = options()$mysql$password)
  
  # Sanitize strings
  users.answers$username <- RMySQL::dbEscapeStrings(db, users.answers$username)
  users.answers$message  <- RMySQL::dbEscapeStrings(db, users.answers$message)
  
  # Get current answers and add new ones
  query <- "SELECT * FROM answers"
  current.answers <- DBI::dbGetQuery(db, query)
  # answers <- current.answers
  answers <- rbind(current.answers, users.answers)
  
  # Retrieve optimates
  scores.matrix <- ReturnScoresMatrix(answers)
  optimal.pairs <- ReturnOptimalPairs(scores.matrix)
  matches <- ReturnMatches(scores.matrix, optimal.pairs)
  
  # Save data to DB
  query.answers <- sprintf(
    "INSERT INTO answers (%s) VALUES %s",
    paste(names(users.answers), collapse = ", "),
    paste0(apply(users.answers, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  )
  
  query.matches <- sprintf(
    "INSERT INTO matches (%s) VALUES %s",
    paste(names(matches), collapse = ", "),
    paste0(apply(matches, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  )
  
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query.answers)
  if(nrow(matches) > 0) DBI::dbGetQuery(db, query.matches)
  
  RMySQL::dbDisconnect(db)
  
  return(invisible(NULL))
}
