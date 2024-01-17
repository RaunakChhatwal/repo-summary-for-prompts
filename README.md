# Summr

This tool allows you to use LLMs (Large Language Models) for coding by displaying the contents of a Git repository in the terminal. It works by reading the tracked files in the repository and outputting their content in a tidy format, which can then be passed to LLM models. This can be helpful for quickly fetching file content from repositories for easier inspection or processing with model-based automation tools.

## Usage

```
summr <repo path> | wl-copy
```

The command above will output the repo's content, which will be copied to the clipboard using `wl-copy`.

## Filtering Files

You can filter the files included in the output by specifying a regular expression (regex) pattern. Files that match the regex pattern will be included, while those that don't will be excluded. The inverse regex pattern can also be provided, in which files matching the inverse pattern will be excluded. This allows for fine-grained control over the selection of files.

```
summr <repo path> -r '<regex pattern>' -v '<inverse regex pattern>' | wl-copy
```

## Dependencies

- Haskell
- GHC

## Build

To compile the code:

```
make
```
