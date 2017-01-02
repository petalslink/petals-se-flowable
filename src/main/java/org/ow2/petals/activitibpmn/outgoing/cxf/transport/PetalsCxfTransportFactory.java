/**
 * Copyright (c) 2014-2016 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.outgoing.cxf.transport;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.transport.AbstractTransportFactory;
import org.apache.cxf.transport.Conduit;
import org.apache.cxf.transport.ConduitInitiator;
import org.apache.cxf.transport.Destination;
import org.apache.cxf.transport.DestinationFactory;
import org.apache.cxf.ws.addressing.AttributedURIType;
import org.apache.cxf.ws.addressing.EndpointReferenceType;

/**
 * A CXF transport to invoke Petals services
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class PetalsCxfTransportFactory extends AbstractTransportFactory implements DestinationFactory, ConduitInitiator {

    public static final String TRANSPORT_ID = "http://apache.org/transports/petals";

    public static final List<String> DEFAULT_NAMESPACES = Arrays.asList(TRANSPORT_ID);

    private static final Logger LOG = LogUtils.getL7dLogger(PetalsCxfTransportFactory.class);

    private static final Set<String> URI_PREFIXES = new HashSet<String>();

    static {
        URI_PREFIXES.add("petals://");
    }

    private Set<String> uriPrefixes = new HashSet<String>(URI_PREFIXES);

    @Override
    public Conduit getConduit(final EndpointInfo targetInfo) throws IOException {
        return new PetalsConduit(this.createReference(targetInfo), this.bus);
    }

    @Override
    public Conduit getConduit(final EndpointInfo localInfo, final EndpointReferenceType target) throws IOException {
        LOG.log(Level.FINE, "Creating conduit for {0}", localInfo.getAddress());
        if (target == null) {
            return new PetalsConduit(this.createReference(localInfo), this.bus);
        } else {
            return new PetalsConduit(target, this.bus);
        }
    }

    @Override
    public Destination getDestination(final EndpointInfo ei) throws IOException {
        // No Destination is required because CXF is used only on a client side
        return null;
    }

    @Override
    public Set<String> getUriPrefixes() {
        return this.uriPrefixes;
    }

    private EndpointReferenceType createReference(final EndpointInfo ei) {
        final EndpointReferenceType epr = new EndpointReferenceType();
        final AttributedURIType address = new AttributedURIType();
        address.setValue(ei.getAddress());
        epr.setAddress(address);
        return epr;
    }

}
