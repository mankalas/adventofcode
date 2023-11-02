# frozen_string_literal: true

require "utils"

module ToC2015
  class Day01 end
end

module ToC2022
  class Day22 end
end

RSpec.describe Utils do
  describe "day_dir" do
    subject { Utils.day_dir(klass) }

    context "ToC2015::Day01" do
      let(:klass) { ToC2015::Day01 }

      it { is_expected.to eq("2015/day_01") }
    end

    context "T0C2022::Day22" do
      let(:klass) { ToC2022::Day22 }

      it { is_expected.to eq("2022/day_22") }
    end
  end
end
