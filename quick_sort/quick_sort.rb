class QuickSort
  attr_reader :array, :result, :comparison_count

  def initialize(input_array, pivot_strategy: :first)
    @array = input_array
    @comparison_count = 0
    @pivot_strategy = pivot_strategy
  end

  def run
    partition(0, @array.length - 1)

    @result = @array
  end

  private

  def partition(start_index, end_index)
    return if end_index - start_index < 1

    @comparison_count += (end_index - start_index)
    case @pivot_strategy
    when :first
    when :last
      swap_values_at(start_index, end_index)
    when :medium
      middle_index = (start_index + end_index) / 2
      numbers = [
        { index: start_index, value: @array[start_index] },
        { index: end_index, value: @array[end_index] },
        { index: middle_index, value: @array[middle_index] }
      ].sort_by { |hash| hash[:value] }

      medium_index = numbers[1][:index]
      swap_values_at(start_index, medium_index)
    end
    @pivot = @array[start_index]
    smaller_part_end_index = start_index + 1

    for j in (start_index + 1)..end_index
      if @array[j] < @pivot
        swap_values_at(smaller_part_end_index, j)
        smaller_part_end_index += 1
      end
    end

    pivot_correct_index = smaller_part_end_index - 1
    swap_values_at(start_index, pivot_correct_index)

    partition(start_index, pivot_correct_index - 1)
    partition(pivot_correct_index + 1, end_index)
  end

  def swap_values_at(i, j)
    @array[i], @array[j] = @array[j], @array[i]
  end
end
