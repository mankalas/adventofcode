# frozen_string_literal: true

require "parslet"

module AoC
  class Parser < Parslet::Parser
    rule(:space) { match('\s').repeat(1) }
    rule(:space?) { space.maybe }
    rule(:integer) { match("[0-9]").repeat(1) }
  end
end
