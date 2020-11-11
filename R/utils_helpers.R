
SaveData <- function(data, databaseName = "optimate_schema", table = "answers") {
  # Connect to the database
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES %s",
    table, 
    paste(names(data), collapse = ", "),
    # paste(data, collapse = "', '")
    paste0(apply(data, 1, function(x) paste0("('", paste0(x, collapse = "', '"), "')")), collapse = ", ")
  )
  
  print(query)
  # Submit the update query and disconnect
  DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
}

LoadData <- function(databaseName = "optimate_schema", table) {
  # Connect to the database
  db <- RMySQL::dbConnect(RMySQL::MySQL(), dbname = databaseName, host = options()$mysql$host, 
                  port = options()$mysql$port, user = options()$mysql$user, 
                  password = options()$mysql$password)
  
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  
  # Submit the fetch query and disconnect
  data <- DBI::dbGetQuery(db, query)
  RMySQL::dbDisconnect(db)
  return(data)
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
    tidyr::pivot_longer(cols = -c("username1"), names_to = "username2", values_to = "match_indicator")
  
  
  matches <- dplyr::left_join(optimal.pairs.long, scores.matrix.long, by = c("username1", "username2")) %>% 
    dplyr::mutate(when_added = format(Sys.time(), "%Y%m%d_%H%M"))
  
  return(matches)  
}

survey.table <- tibble::tribble(
  ~question,           ~answer1,    ~answer2,       ~answer3,          ~answer4,
  "Season",            "Winter",    "Spring",       "Summer",          "Autumn",
  "Colour",            "Red",       "Green",        "Blue",            "Yellow",
  "Superhero",         "Superman",  "Wonder Woman", "Batman",          "Spider-Man",
  "Food",              "Pizza",     "Salad",        "Burger",          "Pasta",
  "TV serie",          "Friends",   "Breaking Bad", "Game of Thrones", "Family Guy",
  "Pet",               "Cat",       "Dog",          "Fish",            "Parrot",
  "Sport",             "Football",  "Tennis",       "Swimming",        "Skiing",
  "Ice cream flavour", "Chocolate", "Vanilla",      "Strawberry",      "Pistachio",
  "Outdoor place",     "Beach",     "Forest",       "City",            "Mountains",
  "Board game",        "Chess",     "Monopoly",     "Scrabble",        "Jenga"
)

ReturnUsersAnswers <- function(username, answers, survey.table) {
  users.answers <- NULL
  
  numerical.answers <- dplyr::filter(answers, username == !!username) %>% 
    dplyr::select(-username) %>% 
    tidyr::pivot_longer(everything(), names_to = "question", values_to = "answer") %>% 
    dplyr::select(answer) %>% 
    dplyr::pull()
  
  for(i in 1:10) {
    users.answers <- rbind(users.answers, (dplyr::pull(survey.table[i, paste0("answer", numerical.answers[i])])))
  }
  
  return(users.answers)
}
