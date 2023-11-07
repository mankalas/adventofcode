# frozen_string_literal: true

require "parslet"

module AoC
  class Parser < Parslet::Parser
    rule(:space) { match('\s').repeat(1) }
    rule(:space?) { space.maybe }
    rule(:integer) { match("[0-9]").repeat(1) }
    rule(:variable) { match("[a-z]").repeat(1) }
  end

  class Transform < Parslet::Transform
    rule(integer: simple(:i)) { i.to_i }
    rule(variable: simple(:v)) { v.to_s }
  end
end
