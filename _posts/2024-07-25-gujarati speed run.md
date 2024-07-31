# Documenting: Learning the Gujarati language (and the Elixir programming language)

**I will learn the [gujarati language](https://en.wikipedia.org/wiki/Gujarati_language) and along with it I will learn the [Elixir programming language](https://en.wikipedia.org/wiki/Elixir_(programming_language)).**

**Start Date:** Thu Jul 25 03:45:54 PM IST 2024

# Day 1

I believe the best way to learn Gujarati is through reading books and conversing with native speakers. I've decided to start with [this book](https://libgen.rs/book/index.php?md5=A59C1369119EC9B919A83FFB52016666). I don't know how good or bad this book is, but I will leave a review once I finish reading it.

For Elixir, there's a [dedicated page](https://elixir-lang.org/learning.html) on their website that provides various learning resources. I will start with the first book on the page, [Elixir in Action](https://libgen.rs/book/index.php?md5=2BDD3BB8C63102C229A207607788F480). I will leave a review on that too once I'm done.

A [video](https://youtu.be/Zclir25LIUw) on letters.

16 pages in, this book sucks. Switching to [this](https://theswissbay.ch/pdf/Books/Linguistics/Mega%20linguistics%20pack/Indo-European/Indo-Aryan/Gujarati%2C%20Teach%20Yourself%20%28Dwyer%29.pdf).

# Day 2

This book begins with the history of Gujarat and its regions. While I could skip this section, I choose not to.

I should start with Elixir too now. I will start read the book. The code in this book is available at [this github page](https://github.com/sasa1977/elixir-in-action) so might want to check it out. 

I should start working out at home daily too. I will start today. 

(Yes I used ChatGPT to indent the code below, no I don't intend to use ChatGPT to indent any actual codes I write.)

```erlang
-module(sum_server).
-behaviour(gen_server).

-export([
    start/0, sum/3,
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3
]).

start() ->
    gen_server:start(?MODULE, [], []).

sum(Server, A, B) ->
    gen_server:call(Server, {sum, A, B}).

init(_) ->
    {ok, undefined}.

handle_call({sum, A, B}, _From, State) ->
    {reply, A + B, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

I can't ever imagine myself writing a code like this just to add two numbers. This is Erlang, apparently, and according to the author it has a lot of boilerplate. This is not an issue with Elixir.

***"It’s often said that Erlang makes hard things easy and easy things hard."***

```erlang
defmodule SumServer do
  use GenServer

  def start do
    GenServer.start(__MODULE__, nil)
  end

  def sum(server, a, b) do
    GenServer.call(server, {:sum, a, b})
  end

  def handle_call({:sum, a, b}, _from, state) do
    {:reply, a + b, state}
  end
end
```

This is the same code, to add two numbers, but written in Elixir. Makes it simpler.

```erlang
defmodule SumServer do
  use ExActor.GenServer

  defstart start

  defcall sum(a, b) do
    reply(a + b)
  end
end
```

This is the same code, to add two numbers, written in Elixir, but using a library called [`ExActor`](https://github.com/sasa1977/exactor), which makes it simpler.

I need not mention all this comes directly from the book, Elixir in Action.

I have been practicing the Gujarati script, the letters are sometimes confusing when I compare them to Hindi.

The book is done with introducing me to the history, pros and cons of Elixir and Erlang. It's time to actually move on to writing some code.

```erlang
iex(4)> [65, 66, 67]
~c"ABC"
iex(5)>
```

Cool stuff, `'ABC'` is basically a list of integers in which each element represents a single character.

# Day 3

Learning about basic conversations in Gujarati. This book has dialogues between people I can read and learn from.

I am done with learning about [`iex`](https://hexdocs.pm/iex/1.14.5/IEx.html), working with variables, organizing code, understanding the type system and how the runtime works. I will learn more about [`mix`](https://hexdocs.pm/elixir/introduction-to-mix.html) sometime later.

I am also learning how to edit video clips I like to shoot randomly so I can upload them to youtube. Maybe one day I will post about them here.

I came to learn about something interesting about Elixir, the `^` operator. It is known as the *pin* operator. Basically it is used to match against a variable:

```erlang
iex(8)> expected_variable = "expected"
"expected"
iex(9)> {^expected_variable, _} = {"expected", "something else"}
{"expected", "something else"}
iex(10)> {^expected_variable, _} = {"not expected", "something else"}
** (MatchError) no match of right hand side value: {"not expected", "something else"}
    (stdlib 6.0.1) erl_eval.erl:652: :erl_eval.expr/6
    iex:10: (file)
iex(10)> 
```

Another case of matching:

```erlang
iex(1)> command = "ping siddhantreal.tech"
"ping siddhantreal.tech"
iex(2)> command = "ping www.github.com"
"ping www.github.com"
iex(3)> "ping " <> url = command
"ping www.github.com"
iex(4)> command
"ping www.github.com"
iex(5)> url
"www.github.com"
iex(6)> 
```

By writing `"ping " <> url = command`, you expect that the command variable is a binary string which starts with `"ping "`. If found true, the <> operator binds the rest of the string in a `url` variable. Otherwise this will raise an error.

# Day 4

Read Elixir in Action a bit, no significant progress in either Elixir or Gujarati.

# Day 5

I have started thinking about a project I will do soon. I will call it `The Nazrin Stock Index`. First I will get done with this book then I will jump to making an API.

Also learning about [Figma](https://figma.com) using [this](https://youtu.be/ezldKx-jPag) tutorial I found on YouTube, hopefully this is enough to get me started. I suck at UI/UX development. Maybe I will get better with time.

# Day 6

Started learning Godot.

# Day 7