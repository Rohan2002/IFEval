# IFEval 

Instruction-Following Evaluation for Large Language Models.

## Summary 
An evaluator for Large Language Model output. This library will help LLM users to verify their output.
The logic implementation is based on a paper written by Google and Yale University and it is found over here.
[IFEval Paper](https://arxiv.org/pdf/2311.07911.pdf)

## Paper

The current workflow of a Large Language Model (LLM)

Natural Language Query (NLQ) -> LLM -> Model Response

The NLQ or prompt is defined by the user and it will have instructions
that the response should follow.

The instructions are defined in ```src/instructions.clj``` file.
There are 25 type of instructions defined in the IFEval paper, and 24 out of the 25 instructions 
were implemented in this library (IFEval rule 14 is vague). 

Suppose we have a prompt, and there are ```N >= 1``` instructions defined in that prompt.

We check if the response strictly or loosely followed the prompt.

Strictly followed: if at least one instruction was not followed then the response did NOT follow the prompt.

Loosely followed: if at least one instruction was followed then the response did follow the prompt.

## Development

To run all tests for this library, run ```lein test```
To run a test suite for a particular test namespace, run ```lein test :only test-namespace```

Run ```lein run``` to get the prompt-level IFEval loose and strict metrics for the given input data.

Prompt-level means in the context of the number of prompts. So if there are 5 prompts then there are be 5 associated
responses. If only 3 responses followed the instructions present in the 3 prompts then the accuracy of prompt-level
will be 3/5.

## Limitations

IFEval Rule 14 in the paper does not make sense to me. If anyone can implement it, I will be happy
to look at it.