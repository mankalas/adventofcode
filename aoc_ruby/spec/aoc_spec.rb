# frozen_string_literal: true

require "2015/day_01"
require "2015/day_02"
require "2015/day_03"
require "2015/day_04"
require "2015/day_05"
require "2015/day_06"
require "2015/day_07"
require "2015/day_08"
require "2015/day_09"
require "2015/day_10"
require "2015/day_11"
require "2015/day_12"
require "2015/day_13"
require "2015/day_14"
require "2015/day_15"
require "2015/day_16"

RSpec.describe "AoC" do
  (2015..2022).each do |year|
    (1..24).each do |day|
      day_s = day.to_s.rjust(2, "0")

      describe year do
        describe "Day #{day_s}" do
          let(:klass) { Object.const_get("AoC#{year}::Day#{day_s}") }
          let(:input) { Utils.input(klass) }

          [1, 2].each do |part|
            describe "Part #{part}" do
              subject { klass.new.send("part#{part}", input).to_s }

              let(:output) { Utils.send("output_part#{part}", klass) }

              it { is_expected.to eq(output) }
            end
          end
        end
      end
    end
  end
end
