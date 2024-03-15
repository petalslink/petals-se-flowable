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

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.cxf.resource.URIResolver;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.ow2.petals.component.framework.su.ServiceUnitDataHandler;

public class InternalBPMNDefinitionURIResolver extends URIResolver {

    public InternalBPMNDefinitionURIResolver(final String baseUriStr, final String uriStr,
            final AbstractServiceUnitManager suMngr) throws IOException {
        super();

        if (baseUriStr.startsWith(InternalBPMNDefinitionURIBuilder.INTERNAL_SCHEME + ":")) {
            this.tryPetals(baseUriStr, uriStr, suMngr);
        } else {
            this.resolve(baseUriStr, uriStr, null);
        }
    }

    private void tryPetals(final String baseUriStr, final String uriStr, final AbstractServiceUnitManager suMngr)
            throws IOException {
        final int i = baseUriStr.indexOf(':');
        assert i > 0;

        final String serviceUnitBase = baseUriStr
                .substring(InternalBPMNDefinitionURIBuilder.INTERNAL_SCHEME.length() + 1);
        final int idx = serviceUnitBase.indexOf(':');
        if (i == -1) {
            throw new IOException("Invalid internal URI: " + baseUriStr);
        }

        final String serviceUnitName = serviceUnitBase.substring(0, idx);
        final String resource = serviceUnitBase.substring(idx + 1);

        final ServiceUnitDataHandler suDH = suMngr.getSUDataHandler(serviceUnitName);
        if (suDH == null) {
            throw new IOException(String.format("Unable to retrieve the service unit '%s'. Perhaps it is not deployed",
                    serviceUnitName));
        }

        try {
            final Method mTryFileSystem = this.getClass().getSuperclass().getDeclaredMethod("tryFileSystem",
                    String.class, String.class);
            mTryFileSystem.setAccessible(true);
            mTryFileSystem.invoke(this,
                    new File(suDH.getInstallRoot(), resource).getAbsolutePath(), uriStr);

        } catch (final NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException e) {
            throw new IOException(e);
        }
    }

}
