# Production Quicknode Logs Parsing

# ETH-WBTC 0.3% Uni v3 Pool on Ethereum Mainnet
pool_address = tolower('0xCBCdF9626bC03E24f779434178A73a0B4bad62eD')

# Pool specific SWAP topic for parsing logs
topic = tolower('0xc42079f94a6350d7e6235f29174924f928cc2ac818eb64fed8004e115fbcca67')

# Data in package is from inception to block 17M
# here use
from_block = 17000000
to_block = 17010000
