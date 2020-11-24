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

/**
 * \file
 * \ingroup propagation
 * if77::If77, if77::If77Config and if77::If77Card class implementations.
 */

#include "if77.h"
#include "if77ata.h"

#include <algorithm>
#include <cstring>    // strncpy
#include <iomanip>
#include <sstream>


namespace if77
{
  
//  --------------------------------------------------------
//  class If77: minimal configuration
//  --------------------------------------------------------
  
If77::If77 (void)
  : H1 (1),
    HAI (1000),
    F (1000)
{
}

If77::If77 (double h1, double hai, double f)
  : H1 (h1),
    HAI (hai),
    F (f)
{
}

If77Config
If77::GetConfig (void) const
{
  If77Config if77;
  if77.HLA = H1;
  if77.HAI = HAI;
  if77.F = F;
  return if77;
}

  
//  --------------------------------------------------------
//  class If77Config: full configuration
//  --------------------------------------------------------
  
/* static */ const std::size_t  If77Config::MAX_TT;
  
If77Config::If77Config (void)
// \todo Hard-code defaults
{
}  // If77Config::If77Config
  
If77Card
If77Config::InputCards (bool newlines /* = true */) const
{
  If77Card ifc {*this};
  ifc.m_newlines = newlines;
  return ifc;
}

double
If77Config::GetLossDbW (double range) const
{
  // Pass the larger value to ATA
  m_dmax = std::max (DMAX, range);
  
  std::stringstream ss;
  // Seems like we should be able to just write into the common block
  // but this doesn't work
  //  ss.rdbuf ()->pubsetbuf (FREAD.CARDS, CARDS_SIZE);

  ss << this->InputCards (false);
  std::strncpy (FREAD.CARDS, ss.str ().c_str (), CARDS_SIZE);

  double lossDbW = ATA (&range);

  return lossDbW;
}


/** Unnamed namespace */
namespace {
  
//  --------------------------------------------------------
//  Pretty printing
//  --------------------------------------------------------
  
// Pretty printing in columns
const std::size_t field_wid = 6;
const std::size_t descr_wid = 52;
#define FIELD std::setw (field_wid)
#define DESCR std::setw (descr_wid)

}  // unnamed namespace


std::ostream &
operator << (std::ostream & os, const If77 & if77)
{
  os << FIELD << "H1" << DESCR << "(lower antenna height above terrain/MSL, m):"
     << if77.H1
     << std::endl;
    
  os << FIELD << "HAI" << DESCR << "(aircraft altitude above MSL, m):"
     << if77.HAI
     << std::endl;

  os << FIELD << "F" << DESCR << "(frequency, MHz):"
     << if77.F
     << std::endl;

  return os;
}
  
std::ostream &
operator << (std::ostream & os, const If77Config & if77)
{
  os << std::left
     << "Card 1:"
     << std::endl;

  os << FIELD << "IK" << DESCR << "(units):";
  switch (if77.IK)
    {
    case If77Config::IK_t::TERMINATE:
      os << "TERMINATE\n";
      break ;
    case If77Config::IK_t::KM_M:
      os << "KM_M (km lateral, meterss altitude)\n";
      break ;
    case If77Config::IK_t::FT_SM:
      os << "FT_SM (statute miles lateral, feet altitude)\n";
      break ;
    case If77Config::IK_t::FT_NM:
      os << "FT_NM (nautical miles lateral, feet altitude)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }; // switch IK

  os << FIELD << "IO" << DESCR << "(type of output):";
  switch (if77.IO)
    {
    case If77Config::IO_t::POWER_AVAILABLE:
      os << "POWER_AVAILABLE\n";
      break ;
    case If77Config::IO_t::POWER_DENSITY:
      os << "POWER_DENSITY\n";
      break ;
    case If77Config::IO_t::TRANSMISSION_LOSS:
      os << "TRANSMISSION_LOSS\n";
      break ;
    default:
      os << "UNKNOWN\n";
    };  // switch IO

  os << FIELD << "IJ" << DESCR << "(aircraft altitude input):";
  switch (if77.IJ)
    {
    case If77Config::IJ_t::USE_ALT:
      os << "USE_ALT (use altitude unit from IK)\n";
      break ;
    case If77Config::IJ_t::USE_DIST:
      os << "USE_DIST (use the lateral unit from IK)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    };  // switch IJ

  os << FIELD << "ILB" << DESCR << "(lobing option):";
  switch (if77.ILB)
    {
    case If77Config::ILB_t::NO_LOBES:
      os << "NO_LOBES (no lobing)\n";
      break ;
    case If77Config::ILB_t::LOBING:
      os << "LOBING (compute lobes)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    };  // switch ILB

  os << FIELD << "KK" << DESCR << "(time availability):";
  switch (if77.KK)
    {
    case If77Config::KK_t::HOURLY:
      os << "HOURLY (hourly median levels)\n";
      break ;
    case If77Config::KK_t::INSTANT:
      os << "INSTANT (instananeous levels)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    };  // switch KK

  std::stringstream ss;
  ss << "(#characters in label, max " << If77Config::MAX_TT << "):";
  os << FIELD << "IA" << DESCR << ss.str ()
     << if77.IA
     << std::endl;

  os << FIELD << "TT" << DESCR << "(label):" << "'" << if77.TT << "'"
     << std::endl;

  os << FIELD << "DMAX" << DESCR << "(max distance in IK lateral units):"
     << if77.DMAX
     << (if77.IGPH > 1 ? " (set to interpolate by IGPH)" : "")
     << std::endl;

  os << FIELD << "JC" << DESCR << "(distance or degrees):"
     << if77.JC
     << (if77.JC > 0 ? " (degrees)" : " (distance, unit from IK)")
     << std::endl;

  os << FIELD << "IGPH" << DESCR << "(use DMAX or calculate):"
     << if77.IGPH
     << (if77.IGPH == 0 ? " (use DMAX)" : " (interpolate)")
     << std::endl;

  os << "\nCard 2:"
     << std::endl;

  os << FIELD << "HLA" << DESCR << "(lower antenna height above MSL):"
     << if77.HLA
     << std::endl;

  os << FIELD << "IFA" << DESCR << "(facility antenna pattern):";
  switch (if77.IFA)
    {
    case If77Config::IFA_t::ISOTROPIC:
      os << "ISOTROPIC\n";
      break ;
    case If77Config::IFA_t::DME:
      os << "DME\n";
      break ;
    case If77Config::IFA_t::TACAN:
      os << "TACAN (RTA-2)\n";
      break ;
    case If77Config::IFA_t::LOOP_4:
      os << "LOOP_4 (4-loop array, cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::LOOP_8:
      os << "LOOP_8 (8-loop array, cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::I_II:
      os << "I_II (cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::JTAC:
      os << "JTAC (with tilted antenna)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IFA

  os << FIELD << "JT" << DESCR << "(facility antenna pattern):";
  switch (if77.JT)
    {
    case If77Config::JT_t::DIRECTIVE:
      os << "DIRECTIVE\n";
      break ;
    case If77Config::JT_t::TRACKING:
      os << "TRACKING\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch JT

  os << FIELD << "IPL" << DESCR << "(facility antenna polarization):";
  switch (if77.IPL)
    {
    case If77Config::IPL_t::HORIZONTAL:
      os << "HORIZONTAL\n";
      break ;
    case If77Config::IPL_t::VERTICAL:
      os << "VERTICAL\n";
      break ;
    case If77Config::IPL_t::CIRCULAR:
      os << "CIRCULAR\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IPL

  os << FIELD << "T1T" << DESCR << "(tilt of facility antenna, deg):"
     << if77.T1T
     << (if77.IFA != If77Config::IFA_t::JTAC ? " (not used, wrong antenna pattern)" : "")
     << std::endl;

  os << FIELD << "HLPBW" << DESCR << "(facility antenna, HALF of -3db power beam width):"
     << if77.HLPBW
     << (if77.IFA != If77Config::IFA_t::JTAC ? " (not used, wrong antenna pattern)" : "")
     << std::endl;

  os << FIELD << "SUR" << DESCR << "(facility site surface above MSL):"
     << if77.SUR
     << std::endl;

  os << FIELD << "IZ" << DESCR << "(rainfall zone):";
  switch (if77.IZ)
    {
    case If77Config::IZ_t::NONE:
      os << "NONE\n";
      break ;
    case If77Config::IZ_t::SAMSON1:
      os << "SAMSON1\n";
      break ;
    case If77Config::IZ_t::SAMSON2:
      os << "SAMSON2\n";
      break ;
    case If77Config::IZ_t::SAMSON3:
      os << "SAMSON3\n";
      break ;
    case If77Config::IZ_t::SAMSON4:
      os << "SAMSON4\n";
      break ;
    case If77Config::IZ_t::SAMSON5:
      os << "SAMSON5\n";
      break ;
    case If77Config::IZ_t::SAMSON6:
      os << "SAMSON6\n";
      break ;
    case If77Config::IZ_t::STORMSIZE:
      os << "STORMSIZE (add 0.5 dB times storm size)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IZ

  os << FIELD << "STS" << DESCR << "(storm size):";
  switch (if77.STS)
    {
    case If77Config::STS_t::STORM_0KM:
      os << "STORM_0KM\n";
      break ;
    case If77Config::STS_t::STORM_5KM:
      os << "STORM_5KM\n";
      break ;
    case If77Config::STS_t::STORM_10KM:
      os << "STORM_10KM\n";
      break ;
    case If77Config::STS_t::STORM_20KM:
      os << "STORM_20KM\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch STS

  os << FIELD << "KD" << DESCR << "(terrain type):";
  switch (if77.KD)
    {
    case If77Config::KD_t::SMOOTH:
      os << "SMOOTH\n";
      break ;
    case If77Config::KD_t::IRREGULAR:
      os << "IRREGULAR\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch KD

  os << FIELD << "KE" << DESCR << "(horizon option):";
  switch (if77.KE)
    {
    case If77Config::KE_t::NONE:
      os << "NONE (no horizon option)\n";
      break ;
    case If77Config::KE_t::ANGLE:
      os << "ANGLE (specified by IDG, IMN and SEC)\n";
      break ;
    case If77Config::KE_t::HEIGHT:
      os << "HEIGHT (specified by HHOI)\n";
      break ;
    case If77Config::KE_t::BOTH:
      os << "BOTH (use both ANGLE HEIGHT parameters)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch KE

  os << FIELD << "DHSI" << DESCR << "(terrain parameter delta h, in ft):"
     << if77.DHSI
     << std::endl;

  os << FIELD << "DHOI" << DESCR << "(facility radio horizon distance in n. mi):"
     << if77.DHOI
     << std::endl;

  os << FIELD << "HHOI" << DESCR << "(facility radio horizon elevation above MSL):"
     << if77.HHOI
     << std::endl;

  os << FIELD << "IDG" << DESCR << "(facility radio horizon angle, deg):"
     << if77.IDG
     << std::endl;

  os << FIELD << "IMN" << DESCR << "(facility radio horizon angle, min):"
     << if77.IMN
     << std::endl;

  os << FIELD << "ISEC" << DESCR << "(facility radio horizon angle, sec):"
     << if77.ISEC
     << std::endl;

  os << FIELD << "DCI" << DESCR << "(facility counterpoise diameter, ft):"
     << if77.DCI
     << std::endl;

  os << FIELD << "HCI" << DESCR << "(facility counterpoise height above surface):"
     << if77.HCI
     << std::endl;

  os << FIELD << "ICC" << DESCR << "(counterpoise reflectivity material):";
  switch (if77.ICC)
    {
    case If77Config::KSC_t::SEA_WATER:
      os << "SEA_WATER\n";
      break ;
    case If77Config::KSC_t::GOOD_GROUND:
      os << "GOOD_GROUND\n";
      break ;
    case If77Config::KSC_t::AVG_GROUND:
      os << "AVG_GROUND\n";
      break ;
    case If77Config::KSC_t::POOR_GROUND:
      os << "POOR_GROUND\n";
      break ;
    case If77Config::KSC_t::FRESH_WATER:
      os << "FRESH_WATER\n";
      break ;
    case If77Config::KSC_t::CONCRETE:
      os << "CONCRETE\n";
      break ;
    case If77Config::KSC_t::METALLIC:
      os << "METALLIC\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch ICC

  
  os << "\nCard 3:"
     << std::endl;

  os << FIELD << "HAI" << DESCR << "(aircraft altitude above MSL):"
     << if77.HAI
     << std::endl;

  os << FIELD << "IAA" << DESCR << "(aircraft antenna pattern):";
  switch (if77.IAA)
    {
    case If77Config::IFA_t::ISOTROPIC:
      os << "ISOTROPIC\n";
      break ;
    case If77Config::IFA_t::DME:
      os << "DME\n";
      break ;
    case If77Config::IFA_t::TACAN:
      os << "TACAN (RTA-2)\n";
      break ;
    case If77Config::IFA_t::LOOP_4:
      os << "LOOP_4 (4-loop array, cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::LOOP_8:
      os << "LOOP_8 (8-loop array, cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::I_II:
      os << "I_II (cosine vertical pattern)\n";
      break ;
    case If77Config::IFA_t::JTAC:
      os << "JTAC (with tilted antenna)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IAA

  os << FIELD << "JS" << DESCR << "(aircraft antenna type):";
  switch (if77.JS)
    {
    case If77Config::JT_t::DIRECTIVE:
      os << "DIRECTIVE\n";
      break ;
    case If77Config::JT_t::TRACKING:
      os << "TRACKING\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch JS

  os << FIELD << "NPL" << DESCR << "(aircraft antenna polarization):";
  switch (if77.NPL)
    {
    case If77Config::IPL_t::HORIZONTAL:
      os << "HORIZONTAL\n";
      break ;
    case If77Config::IPL_t::VERTICAL:
      os << "VERTICAL\n";
      break ;
    case If77Config::IPL_t::CIRCULAR:
      os << "CIRCULAR\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch NPL

  os << FIELD << "T2T" << DESCR << "(tilt of aircraft main beam, in deg):"
     << if77.T2T
     << (if77.IAA != If77Config::IFA_t::JTAC ? " (not used, wrong antenna pattern)" : "")
     << std::endl;

  os << FIELD << "H2PBW" << DESCR << "(aircraft antenna, HALF of -3db power beam width):"
     << if77.H2PBW
     << (if77.IAA != If77Config::IFA_t::JTAC ? " (not used, wrong antenna pattern)" : "")
     << std::endl;
  
  os << FIELD << "ENO" << DESCR << "(surface refractivity, N-units):"
     << if77.ENO
     << std::endl;
  
  os << FIELD << "F" << DESCR << "(frequency, MHz):"
     << if77.F
     << std::endl;
  
  os << FIELD << "EIRP" << DESCR;
  switch (if77.IO)
    {
    case If77Config::IO_t::POWER_AVAILABLE:
      os << "(POWER_AVAILABLE in dbW)";
      break ;
    case If77Config::IO_t::POWER_DENSITY:
      os << "(POWER_DENSITY, in dbW)";
      break ;
    case If77Config::IO_t::TRANSMISSION_LOSS:
      os << "(sum of main beam gains, in dB)";
      break ;
    default:
      os << "(UNKNOWN)";
    };  // switch EIRP
  os << std::endl;

  os << FIELD << "HPFI" << DESCR << "(effective reflective surface elev, MSL):"
     << if77.HPFI
     << std::endl;
  
  os << FIELD << "KSC" << DESCR << "(earth reflectivity material):";
  switch (if77.KSC)
    {
    case If77Config::KSC_t::SEA_WATER:
      os << "SEA_WATER\n";
      break ;
    case If77Config::KSC_t::GOOD_GROUND:
      os << "GOOD_GROUND\n";
      break ;
    case If77Config::KSC_t::AVG_GROUND:
      os << "AVG_GROUND\n";
      break ;
    case If77Config::KSC_t::POOR_GROUND:
      os << "POOR_GROUND\n";
      break ;
    case If77Config::KSC_t::FRESH_WATER:
      os << "FRESH_WATER\n";
      break ;
    case If77Config::KSC_t::CONCRETE:
      os << "CONCRETE\n";
      break ;
    case If77Config::KSC_t::METALLIC:
      os << "METALLIC\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch KSC
  
  os << FIELD << "TP" << DESCR << "(water temp, deg C):";
  switch (if77.TP)
    {
    case If77Config::TP_t::TEMP_0C:
      os << "TEMP_0C\n";
      break ;
    case If77Config::TP_t::TEMP_10C:
      os << "TEMP_10C\n";
      break ;
    case If77Config::TP_t::TEMP_20C:
      os << "TEMP_20C\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch TP

  os << FIELD << "SCK" << DESCR << "(alternate sigma, in ft or m):"
     << if77.SCK
     << std::endl;
  
  os << FIELD << "ISS" << DESCR << "(code for sea state):"
     << if77.ISS
     << std::endl;
  
  os << FIELD << "JM" << DESCR << "(standard or alternate sigma):";
  switch (if77.JM)
    {
    case If77Config::JM_t::STANDARD:
      os << "STANDARD\n";
      break ;
    case If77Config::JM_t::USE_SCK:
      os << "USE_SCK\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch JM

  os << FIELD << "IOS" << DESCR << "(ionospheric scintillation index group):";
  switch (if77.IOS)
    {
    case If77Config::IOS_t::IOS_0:
      os << "IOS_)\n";
      break ;
    case If77Config::IOS_t::IOS_1:
      os << "IOS_1\n";
      break ;
    case If77Config::IOS_t::IOS_2:
      os << "IOS_2\n";
      break ;
    case If77Config::IOS_t::IOS_3:
      os << "IOS_3\n";
      break ;
    case If77Config::IOS_t::IOS_4:
      os << "IOS_4\n";
      break ;
    case If77Config::IOS_t::IOS_5:
      os << "IOS_5\n";
      break ;
    case If77Config::IOS_t::IOS_V:
      os << "IOS_V (variable group)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IOS

  os << FIELD << "IPK" << DESCR << "(frequency scaling for scintillation):";
  switch (if77.IPK)
    {
    case If77Config::IPK_t::NONE:
      os << "NONE\n";
      break ;
    case If77Config::IPK_t::SCALE:
      os << "SCALE\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch IPK
  
  os << FIELD << "JO" << DESCR << "(frequency scaling for scintillation):";
  switch (if77.JO)
    {
    case If77Config::JO_t::NONE:
      os << "NONE\n";
      break ;
    case If77Config::JO_t::SCINTILLATION:
      os << "SCINTILLATION (enabled)\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch JO

  os << FIELD << "KLM" << DESCR << "(climate 1 and time block):";
  switch (if77.KLM)
    {
    case If77Config::KLM_t::CONTINENTAL_ALL:
      os << "CONTINENTAL_ALL (all years)\n";
      break ;
    case If77Config::KLM_t::EQUATORIAL:
      os << "EQUATORIAL\n";
      break ;
    case If77Config::KLM_t::CONTINENTAL_SUBTROPIC:
      os << "CONTINENTAL_SUBTROPIC\n";
      break ;
    case If77Config::KLM_t::MARITIME_SUBTROPIC:
      os << "MARITIME_SUBTROPIC\n";
      break ;
    case If77Config::KLM_t::DESERT:
      os << "DESERT\n";
      break ;
    case If77Config::KLM_t::CONTINENTAL_TEMPERATE:
      os << "CONTINENTAL_TEMPERATE\n";
      break ;
    case If77Config::KLM_t::MARITIME_OVERLAND:
      os << "MARITIME_OVERLAND\n";
      break ;
    case If77Config::KLM_t::MARITIME_OVERSEA:
      os << "MARITIME_OVERSEA\n";
      break ;
    case If77Config::KLM_t::NONE:
      os << "NONE\n";
      break ;
    case If77Config::KLM_t::SUMMER:
      os << "SUMMER\n";
      break ;
    case If77Config::KLM_t::WINTER:
      os << "WINTER\n";
      break ;
    case If77Config::KLM_t::BLOCK_1:
      os << "BLOCK_1\n";
      break ;
    case If77Config::KLM_t::BLOCK_2:
      os << "BLOCK_2\n";
      break ;
    case If77Config::KLM_t::BLOCK_3:
      os << "BLOCK_3\n";
      break ;
    case If77Config::KLM_t::BLOCK_4:
      os << "BLOCK_4\n";
      break ;
    case If77Config::KLM_t::BLOCK_5:
      os << "BLOCK_5\n";
      break ;
    case If77Config::KLM_t::BLOCK_6:
      os << "BLOCK_6\n";
      break ;
    case If77Config::KLM_t::BLOCK_7:
      os << "BLOCK_7\n";
      break ;
    case If77Config::KLM_t::BLOCK_8:
      os << "BLOCK_8\n";
      break ;
    case If77Config::KLM_t::ALL_YEAR:
      os << "ALL_YEAR\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch KLM

  os << FIELD << "MX1" << DESCR << "(climate 1 mixing weight):"
     << if77.MX1
     << std::endl;
  
  os << FIELD << "KLM2" << DESCR << "(climate 2 and time block):";
  switch (if77.KLM2)
    {
    case If77Config::KLM_t::CONTINENTAL_ALL:
      os << "CONTINENTAL_ALL (all years)\n";
      break ;
    case If77Config::KLM_t::EQUATORIAL:
      os << "EQUATORIAL\n";
      break ;
    case If77Config::KLM_t::CONTINENTAL_SUBTROPIC:
      os << "CONTINENTAL_SUBTROPIC\n";
      break ;
    case If77Config::KLM_t::MARITIME_SUBTROPIC:
      os << "MARITIME_SUBTROPIC\n";
      break ;
    case If77Config::KLM_t::DESERT:
      os << "DESERT\n";
      break ;
    case If77Config::KLM_t::CONTINENTAL_TEMPERATE:
      os << "CONTINENTAL_TEMPERATE\n";
      break ;
    case If77Config::KLM_t::MARITIME_OVERLAND:
      os << "MARITIME_OVERLAND\n";
      break ;
    case If77Config::KLM_t::MARITIME_OVERSEA:
      os << "MARITIME_OVERSEA\n";
      break ;
    case If77Config::KLM_t::NONE:
      os << "NONE\n";
      break ;
    case If77Config::KLM_t::SUMMER:
      os << "SUMMER\n";
      break ;
    case If77Config::KLM_t::WINTER:
      os << "WINTER\n";
      break ;
    case If77Config::KLM_t::BLOCK_1:
      os << "BLOCK_1\n";
      break ;
    case If77Config::KLM_t::BLOCK_2:
      os << "BLOCK_2\n";
      break ;
    case If77Config::KLM_t::BLOCK_3:
      os << "BLOCK_3\n";
      break ;
    case If77Config::KLM_t::BLOCK_4:
      os << "BLOCK_4\n";
      break ;
    case If77Config::KLM_t::BLOCK_5:
      os << "BLOCK_5\n";
      break ;
    case If77Config::KLM_t::BLOCK_6:
      os << "BLOCK_6\n";
      break ;
    case If77Config::KLM_t::BLOCK_7:
      os << "BLOCK_7\n";
      break ;
    case If77Config::KLM_t::BLOCK_8:
      os << "BLOCK_8\n";
      break ;
    case If77Config::KLM_t::ALL_YEAR:
      os << "ALL_YEAR\n";
      break ;
    default:
      os << "UNKNOWN\n";
    }  // switch KLM2

  os << FIELD << "MX2" << DESCR << "(climate 2 mixing weight):"
     << if77.MX2
     << std::endl;
    
  return os;
}  // operator << (If77Config)


#undef FIELD
#undef DESCR


std::ostream &
operator << (std::ostream & os, const If77Card & if77Card)
{
  os
     << std::noshowpoint                         // and no trailing decimal point
     << std::setfill (' ')                       // pad with spaces
     << std::right;                              // most fields right aligned


  If77Config if77 (if77Card.if77);
  
  // Card 1
  os << std::setw ( 2) << static_cast<int> (if77.IK)
     << std::setw ( 2) << static_cast<int> (if77.IO)
     << std::setw ( 2) << static_cast<int> (if77.IJ)
     << std::setw ( 2) << static_cast<int> (if77.ILB)
     << std::setw ( 2) << static_cast<int> (if77.KK)
     << std::setw ( 3) << if77.IA
    
     << std::left
     << std::setw (32)
     << if77.TT.substr (0, std::min (if77.IA, if77.MAX_TT))
     << std::right;

  // Use private copy of dmax, in case GetLossDbW() had to modify it
  // Since DMAX can be set without updating m_dmax, fake that here.
  bool resetDmax = false;
  if (if77.m_dmax <= 0)
    {
      if77.m_dmax = if77.DMAX;
      resetDmax = true;
    }
  os << std::setw ( 5) << if77.m_dmax;
  if (resetDmax)
    {
      if77.m_dmax = 0;
    }
          
  os << std::setw ( 2) << if77.JC
     << std::setw ( 2) << if77.IGPH
     << std::string (80-54-1, ' ')  // pad to 79 char
     << (if77Card.m_newlines ? "\n" : " ")
    ;
      

  // Card 2
  os << std::setw ( 6) << if77.HLA
     << std::setw ( 3) << static_cast<int> (if77.IFA)
     << std::setw ( 2) << static_cast<int> (if77.JT)
     << std::setw ( 2) << static_cast<int> (if77.IPL)
     << std::setw ( 5) << if77.T1T
     << std::setw ( 5) << if77.HLPBW
     << std::setw ( 5) << if77.SUR
     << std::setw ( 2) << static_cast<int> (if77.IZ)
     << std::setw ( 3) << static_cast<int> (if77.STS)
     << std::setw ( 2) << static_cast<int> (if77.KD)
     << std::setw ( 2) << static_cast<int> (if77.KE)
     << std::setw ( 6) << if77.DHSI
     << std::setw ( 6) << if77.DHOI
     << std::setw ( 6) << if77.HHOI
     << std::setw ( 3) << if77.IDG
     << std::setw ( 3) << if77.IMN
     << std::setw ( 3) << if77.ISEC
     << std::setw ( 6) << if77.DCI
     << std::setw ( 6) << if77.HCI
     << std::setw ( 2) << static_cast<int> (if77.ICC)
     << std::string (80-78-1, ' ')  // pad to 79 char
     << (if77Card.m_newlines ? "\n" : " ")
    ;

  // Card 3
  os << std::setw ( 6) << if77.HAI
     << std::setw ( 3) << static_cast<int> (if77.IAA)
     << std::setw ( 2) << static_cast<int> (if77.JS)
     << std::setw ( 2) << static_cast<int> (if77.NPL)
     << std::setw ( 5) << if77.T2T
     << std::setw ( 5) << if77.H2PBW
     << std::setw ( 4) << if77.ENO
     << std::setw ( 6) << if77.F
     << std::setw ( 6) << if77.EIRP
     << std::setw ( 6) << if77.HPFI
     << std::setw ( 2) << static_cast<int> (if77.KSC)
     << std::setw ( 5) << static_cast<int> (if77.TP)
     << std::setw ( 5) << if77.SCK
     << std::setw ( 2) << if77.ISS
     << std::setw ( 2) << static_cast<int> (if77.JM)
     << std::setw ( 2) << static_cast<int> (if77.IOS)
     << std::setw ( 2) << static_cast<int> (if77.IPK)
     << std::setw ( 2) << static_cast<int> (if77.JO)
     << std::setw ( 3) << static_cast<int> (if77.KLM)
     << std::setw ( 3) << if77.MX1
     << std::setw ( 3) << static_cast<int> (if77.KLM2)
     << std::setw ( 3) << if77.MX2
     << std::string (80-79-1, ' ')  // pad to 79 char
     << (if77Card.m_newlines ? "\n" : " ")
    ;

  // Termination
  os << std::setw (2) << 0
     << std::string (80-2-1, ' ')  // pad to 79 char
     << (if77Card.m_newlines ? "\n" : " ")
     << std::flush;
      
  return os;

}  // operator << (If77Card)



//  --------------------------------------------------------
//  class If77Card: print Fortran input cards
//  --------------------------------------------------------

If77Card::If77Card (If77Config ifc)
  : if77 (ifc)
{
}
  

}  // namespace if77


