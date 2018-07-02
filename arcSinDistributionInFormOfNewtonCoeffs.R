
# formula corresponding to arcus sinus law, as in p. 79 of Feller, engl. version.
#{{2k} \choose {k}} {{2(n-k)} \choose {n-k}} 2^{-2n}

n <- 10
k <- 0:n

choose(2*k, k) * choose(2*(n-k), n-k) * 2^{-2*n}

# table p 83 Feller polish
#60 second * 60 minutes * 24 hours * 365 days

n <- 60*60*24*365
k <- 60*60*24*(154)

k/n

2*(2/pi) * asin(sqrt(k/n))
