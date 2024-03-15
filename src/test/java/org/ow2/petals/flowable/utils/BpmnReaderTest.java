/**
 * Copyright (c) 2018-2024 Linagora
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
package org.ow2.petals.flowable.utils;

import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.junit.Test;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.flowable.exception.InvalidSuffixForProcessFileException;
import org.ow2.petals.flowable.exception.ProcessDefinitionDeclarationException;

/**
 * Unit tests of {@link BpmnReader}
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class BpmnReaderTest {

    private static final Logger LOG = Logger.getLogger(BpmnReaderTest.class.getName());

    /**
     * Read a BPMN definition described in a resource file having an unsupported suffix
     */
    @Test(expected = InvalidSuffixForProcessFileException.class)
    public void readBpmnModels_withUnsupportedSuffix()
            throws JAXBException, IOException, URISyntaxException, ProcessDefinitionDeclarationException {

        final URL jbiUrl = Thread.currentThread().getContextClassLoader()
                .getResource("parser/model-with-invalid-suffix.jbi.xml");
        assertNotNull(jbiUrl);
        
        final JAXBContext jaxbContext = JAXBContext
                .newInstance(org.ow2.petals.component.framework.jbidescriptor.generated.ObjectFactory.class);
        final Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
        final Jbi jbi = (Jbi) jaxbUnmarshaller.unmarshal(jbiUrl.openStream());
        assertNotNull(jbi);
        assertNotNull(jbi.getServices());
        
        final BpmnReader bpmnReader = new BpmnReader(jbi.getServices(), new File(jbiUrl.toURI()).getParent(), LOG);
        bpmnReader.readBpmnModels();
    }

}
