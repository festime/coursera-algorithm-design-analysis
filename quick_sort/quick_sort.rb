class QuickSort
  attr_reader :array, :result, :comparison_count

  def initialize(input_array)
    @array = input_array
    @comparison_count = 0
  end

  def run
    partition(0, @array.length - 1)

    @result = @array
  end

  private

  def partition(start_index, end_index)
    return if end_index - start_index < 1
    @comparison_count += (end_index - start_index - 1)

    choose_pivot(start_index)
    i = start_index

    for j in (start_index + 1)..end_index
      if @array[j] < @pivot
        swap_values_at(i + 1, j)
        i += 1
      end
    end

    pivot_correct_position = i
    swap_values_at(@pivot_index, pivot_correct_position)

    partition(start_index, pivot_correct_position - 1)
    partition(pivot_correct_position + 1, end_index)
  end

  def swap_values_at(i, j)
    @array[i], @array[j] = @array[j], @array[i]
  end

  def choose_pivot(index)
    @pivot = @array[index]
    @pivot_index = index
  end
end
