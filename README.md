## Database Setup

After installing Postgres, run:

```
createuser yesodB --pwprompt --superuser
# Enter password yesodB when prompted
createdb yesodB
createdb yesodB_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag yesodB:library-only --flag yesodB:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.


departments enum 
	music
	books
	food
	clothes
	furniture 

food department

Food
	type enum beverage | snack | dinner
	brand
	name
	weight
	nutrition
	description

Nutrition
	calories
	fat
	protein 
	carbs 
	caffeine
	specialIngredients


Books
	author 
	genre
	pageCount
	cost 
	new
	

music department 

song 
	name
	length 
	artist
	album
	popularity 
	genre
	tempo 
	
genre enum
	rap
	classical
	electronic
	indie
	jazz

artist 
	name 
	songs list
	albums list 
	bio 
	popularity
	genres list

albums
	name 
	songs list
	description
	popularity 
	genres

Order
	product
	buyer
	seller
	status Status
	orderPlaced OrderPlaced
	orderProcessing OrderProcessing Maybe
	CustomerReview Maybe

CustomerReview
	createdAt
	verified
	rating
	review

Status 
	placed -- when first made by customer
	processing -- the moment the company receives it and begins processing
	shipping 
	completed

OrderPlaced
	createdAt

OrderProcessing 
	createdAt 

OrderShipping
	createdAt
	shipping Shipping
	
Shipping
	createdAt
	origin adress
	destination address

OrderCompleted
	delivered Time
	customerReceived



address 
	x
	y
	

vendor 
	name 
	balance
	currentOrders
	departments
	address Address
	popularity

vendor names enum
	gumptionRecords
	the ringing ear

	wholesome yum
	the big appetite

	the page turner
	spellbound 

	cloth on top

	big time lounge



customers
	names
	shoppingcart 
	currentOrders
	past orders
	balance
	address
	superuser bool 
