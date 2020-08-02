block1 <- list(number=1,
               timestap= "2020-04-17 11:15:00 IST",
               data="London",
               parent=NA)

block2 <- list(number=2,
               timestap= "2020-04-17 11:16:00 IST",
               data="London",
               parent=1)

block3 <- list(number=3,
               timestap= "2020-04-17 11:16:00 IST",
               data="London",
               parent=2)

blockchain = list(block1,block2,block3)

validate <- function(blockchain){
  if(length(blockchain)>=2){
    for (k in 2:length(blockchain)){
      if(blockchain[[k]]$parent != blockchain[[k-1]]$number){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

validate(blockchain)

block4 <- list(number=4,
               timestap= "2020-04-17 11:19:00 IST",
               data="Paris",
               parent=3)
blockchain = list(block1,block2,block3,block4)

validate(blockchain)



# install.packages("digest")
# library(digest)

digest("A block chain is a chain of blocks", "sha256")

block1 <- list(number=1,
               timestap= "2020-04-17 11:15:00 IST",
               data="London",
               parent_hash="0")

block1$hash <- digest(block1,"sha256")

block2 <- list(number=2,
               timestap= "2020-04-17 11:16:00 IST",
               data="Paris",
               parent_hash=block1$hash)

block2$hash <- digest(block2,"sha256")

block3 <- list(number=3,
               timestap= "2020-04-17 11:16:00 IST",
               data="New York",
               parent_hash=block2$hash)

block3$hash <- digest(block3,"sha256")
blockchain = list(block1,block2,block3)


validate <- function(blockchain){
  for (i in 1:length(blockchain)){
    block = blockchain[[i]]
    hash = block$hash
    block$hash = NULL
    hash_expected = digest(block,"sha256")
    if (hash != hash_expected){
      return(FALSE)
    }
  }
  if(length(blockchain) >=2){
    for (i in 2:length(blockchain)){
      if (blockchain[[i]]$parent_hash != blockchain[[i-1]]$hash){
        returb(FALSE)
      }
    }
  }
  return(TRUE)
}

validate(blockchain)


proof_of_work = function(block, difficulty) {
  block$nonce <- 0
  hash = digest(block, "sha256")
  zero <- paste(rep("0", difficulty), collapse="")
  while(substr(hash, 1, difficulty) != zero) {
    block$nonce = block$nonce + 1
    hash = digest(block, "sha256")  
  }
  return(list(hash = hash, nonce = block$nonce))
}

block <- list(number = 1,
              timestamp = "2018-10-01 17:24:00 CEST",
              data = "London",
              hash = "88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6Parent",
              parent_hash = "d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")

proof_of_work(block, 3)


n = 4
iterations = vector("integer", n)
for (i in 1:n) {
  iterations[i] = proof_of_work(block, i)$nonce
}

iterations


mine <- function(previous_block, difficulty = 3, genesis = FALSE){
  
  if (genesis) {
    # define genesis block
    new_block <-  list(number = 1,
                       timestamp = Sys.time(),
                       data = "I'm genesis block",
                       parent_hash = "0")  
  } else {
    # create new block
    new_block <- list(number = previous_block$number + 1,
                      timestamp = Sys.time(),
                      data = paste0("I'm block ", previous_block$number + 1),
                      parent_hash = previous_block$hash)
  }
  # add nonce with PoW
  new_block$nonce <- proof_of_work(new_block, difficulty)$nonce
  # add hash 
  new_block$hash <- digest(new_block, "sha256")
  return(new_block)
}

blockchained = function(difficulty = 3, nblocks = 3) {
  # mine genesis block
  block_genesis = mine(NULL, difficulty, TRUE)   
  # first block is the genesis block
  blockchain <- list(block_genesis)
  
  if (nblocks >= 2) {
    # add new blocks to the chain
    for (i in 2:nblocks){
      blockchain[[i]] <- mine(blockchain[[i-1]], difficulty) 
    }
    
  }
  
  return(blockchain)
}

blockchained(difficulty=4,nblocks = 4)



install.packages("tibble")
library(tibble)
ntrx = 10
sender = sample(LETTERS, ntrx)
receiver = sample(LETTERS, ntrx)
value = round(runif(n = ntrx, min = 0, max = 100), 0)
fee = round(runif(n = ntrx, min = 0, max = 1), 2)

transactions = tibble(sender, receiver, value, fee)


