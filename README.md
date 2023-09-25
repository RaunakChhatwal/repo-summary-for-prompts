# Repo Summary for Prompts

This tool allows you to use LLMs for coding by displaying the contents of a Git repository in the terminal. It works by reading the tracked files in the repository and outputting their content in a tidy format, which can then be passed to LLM models.

## Usage

```
./a.out <repo path> | wl-copy
```

## Dependencies

- Haskell
- GHC

## Build

To compile the code:

```
make
```