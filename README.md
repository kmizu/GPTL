# GPTL - A Programming Language implemented by GPT-4

GPTL is an acronym for "GPT Language". The goal of the GPTL project is to "implement almost everything in GPT-4". Currently, it supports the following features:

- Data types: strings, integers, and booleans.
- Supported operations:
  - Arithmetic operations
  - Comparison operations
  - Assignment operations
- Control structures:
  - Conditional branching
  - Loops
  - Concatenation

Here is an example program in GPTL:

```gptl
let i = 0 in {
  while i < 10 do {
    i := i + 1
  };
  i
}
```

This program increases i from 0 to 10 and returns i at the end. The GPTL project was started to measure the "performance" of GPT-4, but to be honest, it worked better than expected.

GPT-4 was generally good at generating code using familiar methods for language processing system developers (tokenization, parsing, and interpretation). However, as the code grew longer, "careless mistakes" became more noticeable. While it may sound strange to use the term "careless mistakes" to describe AI (or large language models), there were many mistakes that could only be described as such.

For example, in the syntax analysis, "do" tokens were used, but the tokenizer "forgot" to split the "do" token. There were also cases where the code seemed correct at first glance, but turned out to be a type error upon closer inspection. Such "human-like" mistakes were already apparent in GPT-4, but it was valuable to confirm them again through GPTL.

Creating a programming language, not just in terms of design but also implementation, is a challenging task, especially for AI. Although I provided some help through the prompt, I was surprised that GPT-4 was able to generate most of the code successfully.
