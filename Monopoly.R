# Defining Classes --------------------------------------------------------
player = setRefClass("player", fields=list(name="numeric", position="numeric", money="numeric", 
                                           properties="vector"))
property = setRefClass("property", fields=list(name="numeric", position="numeric", price="numeric", 
                                               mortgage_value="numeric", houses="numeric", hotels="numeric", 
                                               rent="vector", house_price="numeric", hotel_price="numeric", 
                                               rent_stage="numeric", mortgaged="logical", owned="logical"))
bank = setRefClass("bank", fields=list(properties="vector", houses="numeric", hotels="numeric"))


# Creating list of all the properties -------------------------------------
# Virginia Ave
Virginia_Ave = property(name=0, position=14, price=160, mortgage_value=80, houses=0, hotels=0, 
                        rent=c(12,24,60,180,500,700,900), house_price=100, hotel_price=100, rent_stage=1, 
                        mortgaged=FALSE, owned=FALSE)
# Indiana Ave
Indiana_Ave = property(name=1, position=23, price=220, mortgage_value=110, houses=0, hotels=0,
                       rent=c(18,36,90,250,700,875,1050), house_price=150, hotel_price=150, rent_stage=1, 
                       mortgaged=FALSE, owned=FALSE)
properties = c(Virginia_Ave, Indiana_Ave)


# Creating list of all the players ----------------------------------------
p1 = player(name=1, position=1, money=1500, properties=vector())
p2 = player(name=2, position=1, money=1500, properties=vector())
p3 = player(name=3, position=1, money=1500, properties=vector())
p4 = player(name=4, position=1, money=1500, properties=vector())
players = c(p1, p2, p3, p4)


# Creating decks ----------------------------------------------------------
community_chest_deck = shuffle_deck(c(1:16))
chance_deck = shuffle_deck(c(1:16))


# Deck functions ----------------------------------------------------------
draw_community_chest = function(x, i) {
  switch (x, update_money(players[i], 100), update_money(players[i], -150), update_pos(players[i], 0, skip=TRUE),
          update_money(players[i], -50), update_money(players[i], 100))
}

# Function to take existing deck and randomly order all cards
shuffle_deck = function(deck) {
  # Goes through the deck card by card and swap each with a randomly selected card
  for(card in c(1:length(deck))) {
    index = ceiling(runif(1, 1, length(deck)))
    temp = deck[card]
    deck[card] = deck[index]
    deck[index] = temp
  }
  return(deck)
}

# Return top n cards from deck (doesn't remove)
draw = function(deck, n = 1) {
  return(deck[(length(deck) - n + 1):length(deck)])
}

# Removes top n cards from deck and returns shortened deck
remove_top = function(deck, n = 1) {
  return(deck[1:(length(deck) - n)])
}


# Player functions --------------------------------------------------------

# By default changes position of provided player by amount, if skip==TRUE
# then it will set position of player to a given location
update_pos = function(p, amount, skip=FALSE) {
  if (skip) {
    p$position = amount
  } else {
    if (amount > 0) {
      p$position = p$position + amount
    } else {
      # Need position to loop around if going backwards from near go
      if (p$position - amount < 0) {
        p$position = max - p$position - amount
      }
    }
  }
}

# Increases/decreases money field of provided player object by amount
update_money = function(p, amount) {
  p$money = p$money + amount
}

# Adds property (title) to player's list of properties
add_property = function(p, title) {
  p$properties = append(p$properties, title)
}


# Property functions ------------------------------------------------------

# Increases houses by num up a max of 4
# If valid
update_houses = function(p, num) {
  if (p$houses + num <= 4) {
    p$houses = p$houses + num
  } else {
    print("Illegal house increase - no change made")
  }
}

# Trades in 4 houses for the given property for a hotel
update_hotels = function(p) {
  if (p$houses == 4) {
    p$hotels = 1
    p$houses = 0
  } else {
    print("Illegal hotel creation - no change made")
  }
}

# Increases rent_stage for given property by stages
update_rent_stage = function(p, stages) {
  if (p$rent_stage + stages <= length(p$rent)) {
    p$rent_stage = p$rent_stage + stages
  } else {
    print("Illegal rent increase - no change made")
  }
}

# Unmortgages mortgaged properties and mortgages unmortgaged properties - equivalent to flipping over the card
update_mortgaged = function(p) {
  p$mortgaged = !p$mortgaged
}

# Basic functions ---------------------------------------------------------

# Rolling dice function
roll = function() {
  num = ceiling(6*runif(1)) + ceiling(6*runif(1))
  return(num)
}


# Simulate game -----------------------------------------------------------


