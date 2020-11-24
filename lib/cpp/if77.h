/* -*- Mode:C++; c-file-style:"gnu"; indent-tabs-mode:nil; -*- */
/*
 * Copyright (c) 2020 Lawrence Livermore National Laboratory
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as
 * published by the Free Software Foundation;
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * Author: Peter D. Barnes, Jr. <pdbarnes@llnl.gov>
 */


#ifndef IF77_CPP_WRAPPER
#define IF77_CPP_WRAPPER

/**
 * \file
 * \ingroup propagation
 * if77::If77, if77::If77Config and if77::If77Card class declarations.
 *
 * \todo Check all referenced page numbers.
 */

#include <iostream>



namespace if77 {

  // Forward declarations
  class If77Config;
  class If77Card;

  
  /**
   * Minimal IF77 input configuration.
   * All other parameters will take on default values, as given in
   * Johnson and Gierhart 1978.pdf, Table 2, p. 89.
   */
  class If77
  {
  public:
    
    /**
     * Default constructor
     * \todo Document defaults.
     */
    If77 (void);

    /**
     * Convenience constructor.
     * \todo Is H1 really HLA?
     *
     * \param [in] h1 Facility or lower antenna height above terrain, in m.
     *                Terrain is assumed to be at sea level.
     * \param [in] hai Aircraft or higher antenna height above mean sea level, in m.
     * \param [in] f Frequency, in MHz.
     */
    If77 (double h1, double hai, double f);

    /**
     * Get the full configuration object.
     * \returns The full \p If77Config representation
     */
    If77Config GetConfig (void) const;
    
    /** Facility or lower antenna height above mean sea level, in m. */
    double H1;

    /** Aircraft or higher antenna above mean sea level, in m */
    double HAI;

    /** Frequency, in MHz. */
    double F;
    
  };  // class If77

  
  /** Pretty print the minimal IF77 configuration. */
  std::ostream & operator << (std::ostream & os, const If77 & if77);
  
  /** The complete IF77 configuration.
   *
   * See
   * Johnson and Gierhart 1978.pdf, section 4, p. 71ff,
   * for detailed definition of these parameters.
   *
   * \todo Initialize with default values, from
   * Johnson and Gierhart 1978.pdf, Table 2, p. 89.
   */
  class If77Config
  {
  public:

    /**
     * Default constructor.
     *
     * See individual parameters for default values.
     */
    If77Config (void);

    /**
     * Convert to card-printing version.
     *
     * \code{.cpp}
     * If77Config if77;
     * 
     * // Print input cards
     * std::cout << if77.InputCards ();
     * \endcode
     *
     * \param [in] newlines Include new lines for pretty printing.
     *             When passing to Fortran don't need new lines.
     */
    If77Card InputCards (bool newlines = true) const;

    /**
     * Get the propagation loss for a given range.
     *
     * \param [in] range The range between the lower (facility)
     *             and upper (aircraft) antennas.
     * \returns The propagation loss, in dbW
     */
    // Not const because it might adjust DMAX for the call
    double GetLossDbW (double range) const;

    /**
     * Card 1
     * @{
     */

    /**
     * Code for units to be used with input.
     * First token is the horizontal (distance) unit.
     * Second token is the altitude unit.
     */
    enum class IK_t {
      TERMINATE = 0,          /**< Terminate a run */
      KM_M,                   /**< km/meters */
      FT_SM,                  /**< ft/statute miles */
      FT_NM                   /**< ft/nautical miles */
    };
    /** Units */
    IK_t IK;

    /** Code for type of output */
    enum class IO_t {
      POWER_AVAILABLE = 1,    /**< Power available */
      POWER_DENSITY,          /**< Power density */
      TRANSMISSION_LOSS       /**< Transmission loss */
    };
    /** Type of output. */
    IO_t IO;

    /** Code for aircraft altitude input */
    enum class IJ_t {
      USE_ALT = 0,            /**< Use the altitude unit from IK. */
      USE_DIST                /**< Use the distance unit from IK. */
    };
    /** Aircraft altitude unit. */
    IJ_t IJ;

    /** Code for lobing options. */
    enum class ILB_t {
      NO_LOBES = 0,           /**< No lobing */
      LOBING                  /**< Lobing */
    };
    /** Lobing option. */
    ILB_t ILB;

    /** Code for time availability options. */
    enum class KK_t {
      HOURLY = 0,             /**< Hourly median levels. */
      INSTANT                 /**< Instantaneous levels. */
    };
    /** Time availability. */
    KK_t KK;

    /** Maximum allowed size of label. */
    static const std::size_t MAX_TT = 32;
    
    /** Number of characters and spaces in label (up to \pname MAX_TT) */
    std::size_t IA;

    /** Label. */
    std::string TT;         

    /**
     * Maximum distance, in the units given by \pname IK.
     *
     * If `IGPH > 1` interpolate and obtain values for this distance.
     */
    double DMAX;

  private:

    /** Copy of DMAX which we can modify in const method GetLossDbW(). */
    mutable double m_dmax;

    friend
    std::ostream & operator << (std::ostream &, const If77Card &);
    
  public:
    
    /**
     * Output distance or degrees.
     * If > 0 output in degrees, otherwise use distance unit given by \pname IK.
     */
    std::size_t JC;

    /** If 0 use \p DMAX, othewise interpolate. */
    std::size_t IGPH;

    /**@}*/  // Card 1

    /**
     * Card 2
     * @{
     */

    /**
     * Facility or lower antenna height above mean sea level.
     * Units as given by \p IK.
     */
    double HLA;
    
    /**
     * Code for facility antenna pattern.
     * Also used for aircraft antenna pattern; \see \p IAA.
     */
    enum class IFA_t {
      ISOTROPIC = 1,          /** Isotropic. */
      DME,                    /** DME */
      TACAN,                  /** TACAN (RTA-2) */
      LOOP_4,                 /** 4-loop array (cosine vertical pattern) */
      LOOP_8,                 /** 8-loop array (cosine vertical pattern) */
      I_II,                   /** I or II (cosine vertical pattern) */
      JTAC                    /** JTAC with tilted antenna */
    };
    /** Facility antenna pattern. */
    IFA_t IFA;

    /**
     * Code for antenna type.
     * Used by both facilty antenna type \p JT and
     * facility antenna type \p JS.*/
    enum class JT_t {
      DIRECTIVE = 0,          /**< Directive */
      TRACKING                /**< Tracking */
    };
    /** Antenna type. */
    JT_t JT;

    /**
     * Code for polarization of facility antenna.
     * This is also used for reflection coefficient and ground constants.
     * Also used for aircraft antenna polarization, \p NPL.
     */
    enum class IPL_t {
      HORIZONTAL = 1,                   /**< Horizontal polarization. */
      VERTICAL,                         /**< Vertical polarization. */
      CIRCULAR                          /**< Circular polarization. */
    };
    /** Facility antenna polarization. */
    IPL_t IPL;

    /**
     * Tilt of the facility antenna main beam, in degrees.
     * Used only for \p JTAC antenna pattern.
     */
    double T1T;

    /**
     * HALF of the half-power beam width of the facility antenna.
     * Used only for \p JTAC antenna pattern.
     */
    double HLPBW;

    /** Elevation of the facility site surface obove MSL. */
    double SUR;

    /**
     * Code for rainfall zones.
     * See Johnson and Gierhart 1978.pdf, Figs. 49-50, pp. 111-112
     */
    enum class IZ_t {
      NONE = 0,                          /**< No consideration. */
      SAMSON1,                           /**< See Samson's maps.. */
      SAMSON2,                           /**< See Samson's maps.. */
      SAMSON3,                           /**< See Samson's maps.. */
      SAMSON4,                           /**< See Samson's maps.. */
      SAMSON5,                           /**< See Samson's maps.. */
      SAMSON6,                           /**< See Samson's maps.. */
      STORMSIZE                          /**< Add 0.5 dB times storm size \p STS to attnuation. */
    };
    /** Rainfall zone. */
    IZ_t IZ;

    /** Code for storm size. */
    enum class STS_t {
      STORM_0KM  =  0,                  /** 0 km storm size (no storm). */
      STORM_5KM  =  5,                  /** 5 km storm size. */
      STORM_10KM = 10,                  /** 10 km storm size. */
      STORM_20KM = 20                   /** 20 km storm size. */
    };
    /** Storm size. */
    STS_t STS;

    /** Code for terrain type options. */
    enum class KD_t {
      SMOOTH = 1,                       /**< Smooth earth. */
      IRREGULAR = 2,                    /**< Irregular terrain. */
    };
    /** Terrain type. */
    KD_t KD;

    /** Code for horizon options. */
    enum class KE_t {
      NONE = 0,                         /**< No horizon option specified. */
      ANGLE,                            /**< Angle specified by \p IDG, \p IMN and \p SEC. */
      HEIGHT,                           /**< Height specified by \p HHOI. */
      BOTH                              /**< Both \p ANGLE and \p HEIGHT are specifice. */
    };
    /**
     * Horizon option.
     * \see \p DHOI, \p HHOI, \p IDG, \p IMN and \p ISEC.
     */
    KE_t KE;

    /**
     * Terrain parameter delta h (ft) from table.
     * See Johnson and Gierhart 1978.pdf, Table 7, p. 101, and
     * Fig. 53, p. 102.
     */
    double DHSI;

    /**
     * Distance to facility radio horizon (n mi.).
     * \note Zero or negative values will result in calculation
     * of this parameter from others (Figure 14).
     * \see \p KE.
     */
    double DHOI;

    /**
     * Eleveation of facility radio horizon above MSL.
     * \see \p KE.
     */
    double HHOI;

    /**
     * Facility radio horizon angle, in degress.
     * \see \p KE.
     */
    std::size_t IDG;
    
    /**
     * Facility radio horizon angle, minutes portion.
     * \see \p KE.
     */
    std::size_t IMN;
    
    /**
     * Facility radio horizon angle, seconds portion.
     * \see \p KE.
     */
    std::size_t ISEC;

    /**
     * Diameter of facility counterpoise (ft).
     * \see \p HCI.
     * \note Zero or negative will cause the program to assume
     * that no counterpoise is present.
     */
    double DCI;

    /**
     * Height of facility counterpoise above site surface.
     * \see \p DCI.
     */
    double HCI;

    // These values are given for \p KSC in \p cards.txt.
    // Forward defined here for use by ICC
    /**
     * Code for earth reflecction material type (Table 2).
     * Also used for counterpoise reflection material type, \p ICC.
     */
    enum class KSC_t {
      SEA_WATER = 1,                    /**< Sea water. */
      GOOD_GROUND,                      /**< Good ground. */
      AVG_GROUND,                       /**< Average ground. */
      POOR_GROUND,                      /**< Poor ground. */
      FRESH_WATER,                      /**< Fresh water. */
      CONCRETE,                         /**< Concrete. */
      METALLIC                          /**< Metallic. */
    };
    /** Code for counterpoise reflection material type. */
    KSC_t ICC;
    
    /**@}*/  // Card 2

    /**
     * Card 3
     * @{
     */

    /** Aircraft or higher antenna above mean sea level. */
    double HAI;

    /** Aircraft antenna pattern. */
    IFA_t IAA;

    /** Aircraft antenna type. */
    JT_t JS;

    /** Aircraft antenna polarization. */
    IPL_t NPL;

    /**
     * Tilt of the aircraft antenna main beam, in degrees.
     * Used only for \p JTAC antenna pattern.
     */
    double T2T;

    /**
     * HALF of the half-power beam width of the aircraft antenna.
     * Used only for \p JTAC antenna pattern.
     */
    double H2PBW;

    /**
     * Surface refractivity referred to sea level (N-units)
     * from Figure 3.
     * See Johnson and Gierhart 1978.pdf, Figs. 51-52, pp. 113-114
     * \note 301 N-units will be used if value not specified
     * or <250 or >400 N-units.
     */
    double ENO;

    /** Frequency (MHz). */
    double F;

    /**
     * Equivalent Isotropically Radiated Power (dBW) for
     * Power Density and Power Available output;
     * sum of the main beam gains in dB for Transmission Loss.
     */
    double EIRP;

    /** Elevation of effective reflection surface above MSL. */
    double HPFI;

    /** Earth reflection material type. */
    KSC_t KSC;

    /** Temperature in celsius of the water. */
    enum class TP_t {
      TEMP_0C  =  0,                    /**< 0° C water. */
      TEMP_10C = 10,                    /**< 10° C water. */
      TEMP_20C = 20                     /**< 20° C water. */
    };
    TP_t TP;

    /**
     * Sigma in feet or meters if you do not use standard.
     * See Johnson and Gierhart 1978.pdf, Table 6, p. 100.
     */
    double SCK;

    /**
     * Code for sea state per table.
     * See Johnson and Gierhart 1978.pdf, Table 6, p. 100.
     */
    std::size_t ISS;

    /** Code for sigma. */
    enum class JM_t {
      STANDARD = 0,                     /**< Use standard. */
      USE_SCK                           /**< Use \p SCK. */
    };
    /** Sigma choice. */
    JM_t JM;

    /**
     * Code for Ionospheric Scintillation index group.
     * See Johnson and Gierhart 1978.pdf, Fig. 47, p. 108
     * for details.
     */
    // Use underlying type long so we can represent variable group. 
    enum class IOS_t : long {
      IOS_0 =  0,                        /**< See Figure 5, GOES Report. */
      IOS_1 =  1,                        /**< See Figure 5, GOES Report. */
      IOS_2 =  2,                        /**< See Figure 5, GOES Report. */
      IOS_3 =  3,                        /**< See Figure 5, GOES Report. */
      IOS_4 =  4,                        /**< See Figure 5, GOES Report. */
      IOS_5 =  5,                        /**< See Figure 5, GOES Report. */
      /**
       * Variable group.
       * See Johnson and Gierhart 1978.pdf, Fig. 48, p. 109
       */
      IOS_V = -1                         
        };
    /** Ionospheric Scintillation index group. */
    IOS_t IOS;

    /**
     * Code for frequency scaling factor, to be used with the
     * scintillation index variable group.
     */
    enum class IPK_t {
      NONE = 0,                         /**< Not used. */
      SCALE                             /**< Scale by 136/f. */
    };
    /** Frequency scaling for scintillation. */
    IPK_t IPK;

    /** Code for scintillation. */
    enum class JO_t {
      NONE = 0,                         /**< No scintillation. */
      SCINTILLATION                     /**< Scintillation enabled. */
    };
    /** Scintillation enabled. */
    JO_t JO;

    /**
     * Code for climate and time blocks.
     * See Johnson and Gierhart 1978.pdf, Table 8, p. 104, for
     * climate types and characteristics.
     * See Johnson and Gierhart 1978.pdf, Table 9, p. 105, for
     * time block definitions.
     */
    enum class KLM_t {
      CONTINENTAL_ALL = 0,              /**< Contintental all years. */
      EQUATORIAL,                       /**< Equatorial. */
      CONTINENTAL_SUBTROPIC,            /**< Continental subtropical. */
      MARITIME_SUBTROPIC,               /**< Maritime subtropical. */
      DESERT,                           /**< Desert. */
      CONTINENTAL_TEMPERATE,            /**< Continental temperate. */
      MARITIME_OVERLAND,                /**< Maritime temperate overland. */
      MARITIME_OVERSEA,                 /**< Maritime temperate overseas. */
      NONE,                             /**< None. */
      SUMMER,                           /**< Summer May-Oct all hours. */
      WINTER,                           /**< Winter Nov-Apr all hours. */
      BLOCK_1,                          /**< Nov-Apr 0600-1300 */
      BLOCK_2,                          /**< Nov-Apr 1300-1800 */
      BLOCK_3,                          /**< Nov-Apr 1800-2400 */
      BLOCK_4,                          /**< May-Oct 0600-1300 */
      BLOCK_5,                          /**< May-Oct 1300-1800 */
      BLOCK_6,                          /**< May-Oct 1800-2400 */
      BLOCK_7,                          /**< May-Oct 0000-0600. */
      BLOCK_8,                          /**< Nov-Apr 0000-0600 */
      ALL_YEAR,                         /**< All year time block. */
    };
    /** Climate 1 and time block. */
    KLM_t KLM;

    /**
     * Climate mixing weighting factor for climate 1.
     * If value is zero no mixing.
     */
    std::size_t MX1;

    /** Climate 2 and time block. */
    KLM_t KLM2;

    /**
     * Climate mixing weighting factor for climate 2.
     * If value is zero no mixing.
     */
    std::size_t MX2;
    
    /**@}*/  // Card 3

  };  // struct _if77

  /** Pretty print the IF77 configuration. */
  std::ostream & operator << (std::ostream & os, const If77Config & if77);

  /**
   * Representation of an \p If77Config which prints Fortran
   * input cards.
   */ 
  class If77Card
  {
  public:
    
    /** Constructor */
    If77Card (If77Config ifc);
    
    /** The configuration. */
    If77Config if77;

    /** Include newlines for pretty printing. */
    bool m_newlines;
    
  };  // class If77Card

  /** Print the IF77 configuration as Fortran input cards. */
  std::ostream & operator << (std::ostream & os, const If77Card & if77Card);
  
   
#undef OSTREAM
  
}  // namespace if77

#endif // IF77_CPP_WRAPPER
