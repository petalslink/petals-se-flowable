/**
 * Copyright (c) 2014-2019 Linagora
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
package org.ow2.petals.flowable.outgoing.cxf.transport;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.cxf.Bus;
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

    private static final Set<String> URI_PREFIXES = new HashSet<>();

    static {
        URI_PREFIXES.add("petals://");
    }

    private final Logger logger;

    private Set<String> uriPrefixes = new HashSet<>(URI_PREFIXES);

    public PetalsCxfTransportFactory(final Logger logger) {
        this.logger = logger;
    }

    @Override
    public Conduit getConduit(final EndpointInfo targetInfo, final Bus bus) throws IOException {
        return new PetalsConduit(createReference(targetInfo), bus, this.logger);
    }

    @Override
    public Conduit getConduit(final EndpointInfo localInfo, final EndpointReferenceType target, final Bus bus)
            throws IOException {
        this.logger.log(Level.FINE, "Creating conduit for {0}", localInfo.getAddress());
        if (target == null) {
            return new PetalsConduit(createReference(localInfo), bus, this.logger);
        } else {
            return new PetalsConduit(target, bus, this.logger);
        }
    }

    @Override
    public Destination getDestination(final EndpointInfo ei, final Bus bus) throws IOException {
        // No Destination is required because CXF is used only on a client side
        return null;
    }

    @Override
    public Set<String> getUriPrefixes() {
        return this.uriPrefixes;
    }

    private static EndpointReferenceType createReference(final EndpointInfo ei) {
        final EndpointReferenceType epr = new EndpointReferenceType();
        final AttributedURIType address = new AttributedURIType();
        address.setValue(ei.getAddress());
        epr.setAddress(address);
        return epr;
    }
}
