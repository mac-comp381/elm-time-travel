# Part 2: Build a time machine

## Step 1: Freeze time

Our goal in this step is to make it so that you can press T to make the time machine freeze the game, then press R to resume it.

To accomplish this, you need to do two things:

- The time machine currently just passes the raw game’s model straight through. Now, instead, you are going to give the time machine its own model, which contains both (1) the raw game’s model and (2) a boolean flag that says whether the game is paused. Take note: the raw game’s entire model is now **wrapped inside the time machine’s model**.
- You will then change the time machine’s update function so that it sets the `paused` flag to true or false in response to key presses, and only calls the raw game’s update function if the `paused` flag is false.

### Add the `paused` flag to the model

Change the `initialStateWithTimeTravel` function so that instead of just using the raw game’s initial state directly, it returns a record with two entries:

- `rawModel`: the raw game’s initial state, and
- `paused`: a boolean flag which starts out `False`.

<details>
  <summary>Click for solution</summary>
    
  ```elm
  initialStateWithTimeTravel rawGame =
    { rawModel = rawGame.initialState
    , paused = False
    }
  ```
</details>
<br>

Note that you can’t pass the time machine model to the raw game! To make everything continue to work, you will need to change the other functions.:

- You need to change `viewWithTimeTravel` so it extracts the `rawModel` to pass to the raw game’s `view` function.
- You will also need to change the `updateWithTimeTravel` function in a similar way, and the replace **only the `rawModel`** inside the time machine model while keeping everything else the same. Use the `{ someRecord | someAttr = newValue }` syntax for that.

<details>
  <summary>Click for solution</summary>
    
  ```elm
  viewWithTimeTravel rawGame computer model =
    rawGame.view computer model.rawModel

  updateWithTimeTravel rawGame computer model =
    { model | rawModel = rawGame.updateState computer model.rawModel }
  ```
</details>
<br>

Test the app; it should still run as normal.

### Implement pausing

Change the `updateWithTimeTravel` function so that it checks the model's `paused` flag.

If the flag is false, it should call `rawGame.updateState` with the current `rawModel`, then return a new time machine state with just the `rawModel` updated and everything else the same.

If the flag is true, update should do nothing; the state of the game remains the same, because it is paused.

Ah, but what does “do nothing” mean here? The function has to return _something_. What should it return?

<details>
  <summary>Click for hint</summary>
    
  “Do nothing” means “return the previous state as the new state.” So if the game is paused, return the whole model as is, without asking `rawGame` to do anything.
</details>
<details>
  <summary>Click for solution</summary>
    
  ```elm
  initialStateWithTimeTravel rawGame =
    { rawModel = rawGame.initialState
    , paused = False
    }
  ```

  ```elm
  updateWithTimeTravel rawGame computer model =
    if model.paused then
      model
    else
      { model | rawModel = rawGame.updateState computer model.rawModel }
  ```
</details>
<br>

The app should _still_ function as normal. But now, if you set `paused = True` in the initial state, the app should still appear, but be permanently frozen!

### Add commands for pausing and unpausing

Add this helper function to the bottom of the file:

```elm
keyPressed keyName computer =
  [ String.toLower keyName
  , String.toUpper keyName
  ]
    |> List.any (\key -> Set.member key computer.keyboard.keys)
```

You can use it like this: `keyPressed "X" computer` → boolean that says whether X is currently being pressed. (The `computer` represents all input from the outside world that the game can use: keys pressed, mouse position, size of the window, etc.)

Now alter `updateWithTimeTravel` so that if T is pressed, it changes `paused` to true (“T” is for “Time travel”), if R is pressed, it changes `paused` to false (“R” is for “Resumse”), and otherwise it proceeds to the same behavior as before.

<details>
  <summary>Click for hint</summary>
    
  `updateWithTimeTravel` will consit of a 4-choice if / else chain:

  - T pressed → pause
  - R pressed → resumes
  - paused → do nothing
  - otherwise → let the raw game update its state

  The Elm if / else syntax is:

  ```elm
  if condition then
    result
  else if other condition then
    other result
  else
    ...
  ```
</details>
<br>

Test this. You should be able to use T and R to pause and unpause. And it should work exactly the same with either Mario or Asteroids! (Remember that you can switch between them by editing `Main.elm`.)

Congratulations! You are now controlling time.

### Add some help text to the UI

The time machine is now going to add graphcis of its own on top of the game UI.

Add this constant to your `TimeTravel` module (at the top level, either top or bottom of the file):
```elm
controlBarHeight = 64
```

Now modify `viewWithTimeTravel` so that it adds a `words` element to the list of shapes returned by the game:

```elm
viewWithTimeTravel rawGame computer model =
  let
    helpMessage =
        if model.paused then
          "Press R to resume"
        else
          "Press T to time travel"
  in
    (rawGame.view computer model.rawModel) ++
      [ words white helpMessage
          |> move 0 (computer.screen.top - controlBarHeight / 2)
      ]
```

Things to study in that code:

- How does the `let … in …` language construct work?
- Why `++`?
- How did the designers of this graphics API design `words` and `move` so they work nicely with the pipeline operator?

Next step: [Save history](2-time-machine-step-2.md)
