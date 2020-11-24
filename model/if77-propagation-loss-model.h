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
#ifndef IF77_PROPAGATION_LOSS_MODEL_H
#define IF77_PROPAGATION_LOSS_MODEL_H

#include "if77.h" //header defining If77Config

#include "ns3/object.h"
#include "ns3/propagation-loss-model.h"
#include "ns3/ptr.h"

/**
 * \file
 * \ingroup if77
 * \ingroup propagation
 * ns3::If77PropagationLossModel class declaration
 */
namespace ns3 {

//Forward declaration
class MobilityModel;

class If77PropagationLossModel : public PropagationLossModel
{
public:
    /**
     * Get type information for this class
     *
     * \return The TypeId for this class
     */
    static TypeId GetTypeId ();

    /**
     * Default Constructor
     */
    If77PropagationLossModel ();

    /**
     * Destructor
     */
    virtual ~If77PropagationLossModel ();

    /**
     * Get a writable reference to the default configuration
     *
     * This reference can be used to modify the settings used to calculate
     * propagation loss
     *
     * \return Reference to the default configuration
     */
    if77::If77Config& GetDefaultConfig ();

    /**
     * Get a read only copy of the default configuration
     *
     * \return A const reference to the default configuration
     */
    const if77::If77Config& GetDefaultConfig () const;

private:
    //Inherited Function
    virtual double DoCalcRxPower (double txPowerDbm,
                                  Ptr<MobilityModel> a,
                                  Ptr<MobilityModel> b) const;
    virtual int64_t DoAssignStreams (int64_t stream);

private:
    /**
     * Frequency of the transmission
     */
    double m_frequency;

    if77::If77Config m_defaultConfig;
};

}

#endif /* IF77_PROPAGATION_H */

