program exoplanet_transit_modeling
		implicit none
		integer, parameter :: num_periods = 3  ! Number of orbital periods to simulate
		real(8), parameter :: distance_ly = 1070.0  ! Distance from Earth in light years
		real(8), parameter :: distance_km = distance_ly * 9.461e12  ! Convert light years to km
		real(8), parameter :: star_radius = 0.411  ! Radius of K2-18 in solar radii
		real(8), parameter :: exoplanet_radii(4) = [0.050, 0.1, 0.15, 0.2]  ! Array of exoplanet radii to simulate (in solar radii)
		real(8), parameter :: orbital_period = 33.0  ! Orbital period of K2-18b in days
		real(8), parameter :: noise_range = 0.05  ! Noise range +/- 5% of brightness

		! Define variables
		real(8) :: time, brightness
		real(8) :: time_step, transit_duration
		integer :: i, j, unit_number
		real(8) :: exoplanet_radius, transit_depth, noise
		real(8), allocatable :: brightness_data(:), time_data(:)
		character(len=100) :: filename

		! Calculate time step and transit duration
		time_step = orbital_period / 1000.0  ! Set the time step
		transit_duration = orbital_period / 10.0  ! Transit duration as a fraction of orbital period

		! Allocate arrays for time and brightness data
		allocate(time_data(num_periods * 1000), brightness_data(num_periods * 1000))

		! Iterate through each exoplanet radius
		do i = 1, size(exoplanet_radii)
				exoplanet_radius = exoplanet_radii(i)

				! Initialize time and brightness arrays
				time = 0.0
				brightness = 1.0

				! Simulate for multiple orbital periods
				do j = 1, num_periods * 1000
						! Calculate time
						time = j * time_step

						! Simulate brightness change due to transit
						if (mod(time, orbital_period) < transit_duration) then
								transit_depth = (exoplanet_radius / star_radius)**2
								brightness = 1.0 - transit_depth
						else
								brightness = 1.0
						end if

						! Add random noise
						noise = (2.0 * rand() - 1.0) * noise_range
						brightness = brightness + noise

						! Store time and brightness data
						time_data(j) = time
						brightness_data(j) = brightness
				end do

				! Generate filename for the current exoplanet radius
				write(filename, '(A, F4.3, A)') 'exoplanet_radius_', exoplanet_radius, '.csv'

				! Open the file for writing
				open(newunit=unit_number, file=filename, action='write', status='replace')

				! Write header to CSV file
				write(unit_number, '(A)') 'Time (days),Brightness'

				! Write time and brightness data to the file
				do j = 1, num_periods * 1000
						write(unit_number, '(F8.3, A, F8.3)') time_data(j), ',', brightness_data(j)
				end do

				! Close the file
				close(unit_number)

				! Output the filename of the CSV file saved
				print *, "Data saved to file:", filename
		end do

		! Deallocate arrays
		deallocate(time_data, brightness_data)

end program exoplanet_transit_modeling