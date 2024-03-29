% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tick_to_price.R
\name{tick_to_price}
\alias{tick_to_price}
\title{Tick to Price}
\usage{
tick_to_price(tick, decimal_adjustment = 1, yx = TRUE)
}
\arguments{
\item{tick}{The numeric tick, e.g., 204232.}

\item{decimal_adjustment}{The difference in the tokens decimals, e.g., 1e10 for ETH vs BTC. 1e12
for USDC vs ETH.}

\item{yx}{Whether to return tick in Token 1 / Token 0 format or inverted. The USDC/ETH 0.05\% pool
on Ethereum mainnet at the contract level is ETH per USDC, but this is harder to interpret than the inverse.
Default is TRUE.}
}
\value{
A numeric price in desired format. See ?get_closest_tick with `tick_spacing = 1` for the reverse process.
}
\description{
Converts a Uniswap v3 tick to a human readable price accounting
for differences in the token decimals and whether price is desired in
Token 1 / Token 0 (y/x) or inverted (x/y) format. See ?get_closest_tick for the reverse of this function.
}
\examples{
 # 1,351.327 USDC per ETH (i.e., yx = FALSE because pool is actually ETH/USDC);
 # 6 decimals vs 18 decimals -> 1e12 adjustment.
 tick_to_price(204232, decimal_adjustment = 1e12, yx = FALSE)

 # 19.98232 ETH per WBTC (i.e., yx = TRUE); 8 decimals vs 18 decimals -> 1e10 adjustment.
tick_to_price(260220, decimal_adjustment = 1e10, yx = TRUE)

 # You can invert results as desired as equivalent to switching yx.
tick_to_price(260220, decimal_adjustment = 1e10, yx = FALSE)^-1 /
 tick_to_price(260220, decimal_adjustment = 1e10, yx = TRUE) - 1 < 0.000001
}
