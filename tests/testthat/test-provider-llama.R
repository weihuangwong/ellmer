test_that("chat_llama() creates a Chat object", {
  skip_if_offline()
  
  chat <- chat_llama("http://localhost:8000")
  expect_s7_class(chat, "Chat")
  expect_s7_class(chat$provider, "ProviderLlama")
  
  expect_equal(chat$provider@base_url, "http://localhost:8000")
})

test_that("ProviderLlama handles NULL roles correctly", {
  provider <- ProviderLlama(
    name = "Llama Server",
    base_url = "http://localhost:8000",
    model = "llama3",
    params = NULL,
    extra_args = list(),
    api_key = NULL
  )
  
  # Simulate a response from llama-server with missing role field
  result <- list(
    choices = list(
      list(
        message = list(
          content = list("Hello, how can I help you today?")
        )
      )
    ),
    usage = list(
      prompt_tokens = 10,
      completion_tokens = 20
    )
  )
  
  # Test that value_turn handles this correctly
  turn <- value_turn(provider, result)
  expect_s7_class(turn, "Turn")
  expect_equal(turn@role, "assistant")
  expect_equal(turn@text, "Hello, how can I help you today?")
})