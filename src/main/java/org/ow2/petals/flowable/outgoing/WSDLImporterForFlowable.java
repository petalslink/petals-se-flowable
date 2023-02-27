/**
 * Copyright (c) 2018-2023 Linagora
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
package org.ow2.petals.flowable.outgoing;

import java.io.IOException;

import org.apache.cxf.resource.URIResolver;
import org.flowable.bpmn.model.Import;
import org.flowable.engine.impl.webservice.CxfWSDLImporter;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.ow2.petals.flowable.utils.InternalBPMNDefinitionURIBuilder;
import org.ow2.petals.flowable.utils.InternalBPMNDefinitionURIResolver;

/**
 * <p>
 * WSDL Importer for Flowable to be able to resolve import against a source system id based on our internal URI.
 * </p>
 * 
 * @see InternalBPMNDefinitionURIBuilder
 * 
 * @author Christophe DENEUX - Linagora
 */
public class WSDLImporterForFlowable extends CxfWSDLImporter {

    private final AbstractServiceUnitManager suMngr;

    public WSDLImporterForFlowable(final AbstractServiceUnitManager suMngr) {
        super();
        this.suMngr = suMngr;
    }

    @Override
    protected URIResolver createUriResolver(final String sourceSystemId, final Import theImport) throws IOException {
        return new InternalBPMNDefinitionURIResolver(sourceSystemId, theImport.getLocation(), this.suMngr);
    }
}
