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
 * Author: Mathew Bielejeski <bielejeski1@llnl.gov> 
 */

#include "if77-propagation-loss-model.h"

#include "ns3/double.h"
#include "ns3/log.h"
#include "ns3/mobility-model.h"

/**
 * \file
 * \ingroup propagation
 * ns3::If77PropagationLossModel implementation
 */

namespace {

    if77::If77Config CreateDefaultConfig ()
    {
        using namespace if77;

        If77Config config;

        config.IK    = If77Config::IK_t::KM_M;
        config.IO    = If77Config::IO_t::POWER_AVAILABLE;
        config.IJ    = If77Config::IJ_t::USE_ALT;
        config.ILB   = If77Config::ILB_t::NO_LOBES;
        config.KK    = If77Config::KK_t::INSTANT;
        config.IA    = 0;
        config.TT    = "VOR";
        config.DMAX  = 260;
        config.JC    = 0;
        config.IGPH  = 0;

        // Card 2:
        config.HLA   = 0;
        config.IFA   = If77Config::IFA_t::LOOP_4;
        config.JT    = If77Config::JT_t::DIRECTIVE;
        config.IPL   = If77Config::IPL_t::HORIZONTAL;
        config.T1T   = 0;
        config.HLPBW = 0;
        config.SUR   = 0;
        config.IZ    = If77Config::IZ_t::NONE;
        config.STS   = If77Config::STS_t::STORM_0KM;
        config.KD    = If77Config::KD_t::SMOOTH;
        config.KE    = If77Config::KE_t::NONE;
        config.DHSI  = 0;
        config.DHOI  = 0;
        config.HHOI  = 0;
        config.IDG   = 0;
        config.IMN   = 0;
        config.ISEC  = 0;
        config.DCI   = 52;
        config.HCI   = 12;
        config.ICC   = If77Config::KSC_t::METALLIC;

        // Card 3:
        config.HAI   = 0;
        config.IAA   = If77Config::IFA_t::ISOTROPIC;
        config.JS    = If77Config::JT_t::DIRECTIVE;
        config.NPL   = If77Config::IPL_t::HORIZONTAL;
        config.T2T   = 0;
        config.H2PBW = 0;
        config.ENO   = 301;
        config.F     = 0;
        config.EIRP  = 0;
        config.HPFI  = 0;
        config.KSC   = If77Config::KSC_t::AVG_GROUND;
        config.TP    = If77Config::TP_t::TEMP_0C;
        config.SCK   = 0;
        config.ISS   = 0;
        config.JM    = If77Config::JM_t::STANDARD;
        config.IOS   = If77Config::IOS_t::IOS_0;
        config.IPK   = If77Config::IPK_t::NONE;
        config.JO    = If77Config::JO_t::NONE;
        config.KLM   = If77Config::KLM_t::CONTINENTAL_ALL;
        config.MX1   = 0;
        config.KLM2  = If77Config::KLM_t::CONTINENTAL_ALL;
        config.MX2   = 0;

        return config;
    }

}

namespace ns3 {

NS_LOG_COMPONENT_DEFINE ("If77PropagationLossModel");

NS_OBJECT_ENSURE_REGISTERED (If77PropagationLossModel);

TypeId
If77PropagationLossModel::GetTypeId ()
{
    static TypeId tid = TypeId ("ns3::If77PropagationLossModel")
        .SetParent<PropagationLossModel> ()
        .SetGroupName ("Propagation")
        .AddConstructor<If77PropagationLossModel> ()
        .AddAttribute ("Frequency",
                        "The transmission frequency in MHz (default is 1Mhz)",
                        DoubleValue (1),
                        MakeDoubleAccessor (&If77PropagationLossModel::m_frequency),
                        MakeDoubleChecker<double> ())
        ;

    return tid;
}

If77PropagationLossModel::If77PropagationLossModel ()
    :   m_frequency (1),
        m_defaultConfig (CreateDefaultConfig ())
{
    NS_LOG_FUNCTION (this);
}

If77PropagationLossModel::~If77PropagationLossModel ()
{
    NS_LOG_FUNCTION (this);
}

if77::If77Config&
If77PropagationLossModel::GetDefaultConfig ()
{
    NS_LOG_FUNCTION (this);

    return m_defaultConfig;
}

const if77::If77Config&
If77PropagationLossModel::GetDefaultConfig () const
{
    NS_LOG_FUNCTION (this);

    return m_defaultConfig;
}

double 
If77PropagationLossModel::DoCalcRxPower (double txPowerDbm,
                                          Ptr<MobilityModel> a,
                                          Ptr<MobilityModel> b) const
{
    const double POWER_CONVERSION = 30; //used to convert between dBm <-> dBw

    const double MPerKm = 1000; // meters per kilometer

    NS_LOG_FUNCTION (this << txPowerDbm << a << b);

    Vector lowPosition = a->GetPosition ();
    Vector highPosition = b->GetPosition (); 

    if (lowPosition.z > highPosition.z)
    {
        NS_LOG_LOGIC ("Swapping which node has the lower position");
        std::swap(lowPosition, highPosition);
    }

    if77::If77Config config = m_defaultConfig; 

    //Override any user changes to these settings
    config.IK = if77::If77Config::IK_t::KM_M;
    config.IO  = if77::If77Config::IO_t::POWER_AVAILABLE;

    config.HLA = lowPosition.z;     //height of transmitter (in meters)
    config.HAI = highPosition.z;    //height of receiver (in meters)
    config.F = m_frequency;         //frequency (in Mhz)
    config.EIRP = txPowerDbm - POWER_CONVERSION;  //Convert dBm to dBw

    lowPosition.z = 0;
    highPosition.z = 0;

    double range = CalculateDistance (lowPosition, highPosition);

    NS_LOG_DEBUG ("Range between nodes: " << range << " m");

    auto rxPower = config.GetLossDbW (range / MPerKm);

    //convert dBW back to dBm
    rxPower += POWER_CONVERSION;

    NS_LOG_DEBUG ("Calculated Rx Power: " << rxPower << " (dBm)");

    return rxPower;
}

int64_t
If77PropagationLossModel::DoAssignStreams (int64_t stream)
{
    NS_LOG_FUNCTION (this << stream);

    return 0;
}

}   //  ns3 namespace

