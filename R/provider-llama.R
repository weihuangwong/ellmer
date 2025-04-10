#' @include provider-openai.R
#' @include content.R
#' @rdname chat_llama
NULL

#' Chat with a llama-server
#'
#' @description
#' This function connects to a llama-server instance that provides an OpenAI-compatible API.
#' Note that some llama-server implementations may not include role fields in their 
#' responses, which this function handles automatically.
#'
#' @inheritParams chat_openai
#' @param api_key Optional API key if your llama-server requires authentication
#' @inherit chat_openai return
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_llama("http://localhost:8000")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_llama <- function(
  base_url,
  system_prompt = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  api_key = NULL,
  echo = NULL
) {
  check_string(base_url)
  if (!is.null(api_key)) {
    check_string(api_key)
  }
  echo <- check_echo(echo)

  provider <- ProviderLlama(
    name = "Llama Server",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderLlama <- new_class(
  "ProviderLlama",
  parent = ProviderOpenAI,
  package = "ellmer",
)

# Explicit role handling for llama-server
method(value_turn, ProviderLlama) <- function(
  provider,
  result,
  has_type = FALSE
) {
  if (has_name(result$choices[[1]], "delta")) {
    # streaming
    message <- result$choices[[1]]$delta
  } else {
    message <- result$choices[[1]]$message
  }

  if (has_type) {
    json <- jsonlite::parse_json(message$content[[1]])
    content <- list(ContentJson(json))
  } else {
    content <- lapply(message$content, as_content)
  }
  if (has_name(message, "tool_calls")) {
    calls <- lapply(message$tool_calls, function(call) {
      name <- call$`function`$name
      args <- jsonlite::parse_json(call$`function`$arguments)
      ContentToolRequest(name = name, arguments = args, id = call$id)
    })
    content <- c(content, calls)
  }
  tokens <- tokens_log(
    provider,
    input = result$usage$prompt_tokens,
    output = result$usage$completion_tokens
  )
  
  # Always provide a default role for llama-server responses
  # as they often don't include roles in responses
  Turn("assistant", content, json = result, tokens = tokens)
}

# Optional API key for base_request
method(base_request, ProviderLlama) <- function(provider) {
  req <- request(provider@base_url)
  if (!is.null(provider@api_key)) {
    req <- req_auth_bearer_token(req, provider@api_key)
  }
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}